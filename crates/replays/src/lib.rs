use std::{io::{BufWriter, Write}, path::PathBuf, sync::{mpsc::Sender, LazyLock}};

use serde::{Deserialize, Serialize};
use serde_json::Value;

pub struct ReplayHandle {
    tx: Sender<ReplayMsg>,
}

enum ReplayMsg {
    AddEvent(ReplayLog),
    StartReplayLog,
    ReplayFile(PathBuf),
    ReplayBuffer(Box<dyn Write + 'static + Send>),
    Finish,
    GetReplayCopy(tokio::sync::oneshot::Sender<Vec<ReplayLog>>),
}

/// a replay log represents how replay events
/// are stored and serialized. it simply stores
/// the timestamp, and the replay event.
/// the replay log should be serialized onto a single line
/// and a replay can be replayed efficiently by scanning across lines
/// rather than needing to deserialize the entire replay into memory at once.
/// users viewing a replay can get a top-level view without needing to expand into
/// nested views that may require reading more lines into memory
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ReplayLog<T = ReplayEvent> {
    /// a unix epoch timestamp. the number of milliseconds
    /// from the epoch (01-01-1970). milliseconds are returned by SystemTime::duration_since
    /// as a u128. we remove the upper 64 bits to represent as a u64. this means
    /// this code will stop working in
    /// 18446744073709551615 / 31556952000 = 584554049 years after 1970
    /// (u64::MAX / number of milliseconds in a year)
    pub t_ms: u64,
    /// log data. "d" to keep the logs small
    pub d: T,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "t", content = "c")]
pub enum ReplayEvent {
    /// emitted when a resource is read into state from the .dpl file
    /// the value represents its current input serialized as JSON.
    /// NOTE: if there's dynamic lookups these are represented using
    /// the special json_with_positions PATH_QUERY_KEY
    ResourceRead { resource_name: String, value: Value },
    /// emitted when resources are being changed. this event
    /// contains the decision made of what to do with this resource
    TrChange { resource_name: String, change_type: TrChangeType },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum TrChangeType {
    Create,
    Update,
    Delete,
    Unmodified,
}

pub fn create_replay_handle() -> ReplayHandle {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut replay_has_started = false;
        let mut replay_logs: Vec<ReplayLog> = vec![];
        let mut replay_save_file: Option<Box<dyn Write>> = None;
        let flush_size = 100;
        while let Ok(msg) = rx.recv() {
            if !replay_has_started {
                if let ReplayMsg::StartReplayLog = &msg {
                    replay_has_started = true;
                }
                // if replay hasnt started yet, drop any message that comes in
                continue;
            }
            match msg {
                ReplayMsg::AddEvent(replay_event) => {
                    replay_logs.push(replay_event);
                    if replay_logs.len() >= flush_size {
                        try_flush_replay(&mut replay_logs, &mut replay_save_file);
                    }
                }
                ReplayMsg::StartReplayLog => {},
                ReplayMsg::Finish => {
                    break;
                }
                ReplayMsg::ReplayBuffer(buf) => {
                    if replay_save_file.is_some() {
                        // dont override buffer if already set
                        continue;
                    }
                    replay_save_file = Some(buf);
                }
                ReplayMsg::ReplayFile(fp) => {
                    if replay_save_file.is_some() {
                        // dont override buffer if already set
                        continue;
                    }
                    match std::fs::File::create(&fp) {
                        Ok(fh) => {
                            let buffer = BufWriter::new(fh);
                            replay_save_file = Some(Box::new(buffer));
                            try_flush_replay(&mut replay_logs, &mut replay_save_file);
                        }
                        Err(e) => {
                            eprintln!("Failed to open replay file '{:?}': {:?}", fp, e);
                        }
                    }
                }
                ReplayMsg::GetReplayCopy(tx) => {
                    let replay_copy = replay_logs.clone();
                    let _ = tx.send(replay_copy);
                }
            }
        }
        try_flush_replay(&mut replay_logs, &mut replay_save_file);
    });
    ReplayHandle { tx }
}

/// tries to flush all logs into the buffer
pub fn try_flush_replay<W: Write>(
    logs: &mut Vec<ReplayLog>,
    replay_save_file: &mut Option<W>
) {
    let buffer = if let Some(b) = replay_save_file {
        b
    } else { return };

    // iterate all logs and serialize them and write as json line to the buffer
    // any failures simply log to stderr
    for log in logs.iter() {
        let log_str = match serde_json::to_string(&log) {
            Ok(o) => o,
            Err(e) => {
                eprintln!("dropping replay event: failed to serialize '{:?}': {:?}", log, e);
                continue;
            }
        };
        if let Err(e) = buffer.write_all(log_str.as_bytes()) {
            eprintln!("dropping replay event: failed to write to output buffer {:?}", e);
        }
        let _ = buffer.write_all(b"\n");
    }
    let _ = buffer.flush();
    // finally, clear the logs vec so future pushes
    // go to the beginning
    logs.clear();
}

static GLOBAL_REPLAY: LazyLock<ReplayHandle> = LazyLock::new(|| {
    create_replay_handle()
});

impl ReplayHandle {
    /// sends a message in the channel to the replay background thread.
    /// this function will panic if the background thread's receiver has been dropped
    /// which should never happen. the only time this can happen is if there's a panic
    /// in the replay background thread, and therefore we want to panic and alert the user
    /// to file a bug report in such an event.
    fn send_message(&self, msg: ReplayMsg) {
        self.tx.send(msg)
            .expect("replay background thread closed unexpectedly. there is likely a bug in the replay handler");
    }
}

pub fn get_epoch_ms_timestamp() -> u64 {
    let now = std::time::SystemTime::now();
    let millis = now.duration_since(std::time::SystemTime::UNIX_EPOCH)
        .map(|d| d.as_millis()).unwrap_or_default();
    millis as u64
}

pub fn add_replay_event(event: ReplayEvent) {
    let grp = &*GLOBAL_REPLAY;
    let log = ReplayLog {
        t_ms: get_epoch_ms_timestamp(),
        d: event,
    };
    grp.send_message(ReplayMsg::AddEvent(log));
}

pub fn resource_read<S: Into<String>>(resource_name: S, value: Value) {
    let resource_name = resource_name.into();
    add_replay_event(ReplayEvent::ResourceRead { resource_name, value });
}

pub fn resource_updated<S: Into<String>>(resource_name: S) {
    let resource_name = resource_name.into();
    add_replay_event(ReplayEvent::TrChange { resource_name, change_type: TrChangeType::Update });
}

pub fn resource_created<S: Into<String>>(resource_name: S) {
    let resource_name = resource_name.into();
    add_replay_event(ReplayEvent::TrChange { resource_name, change_type: TrChangeType::Create });
}

pub fn resource_deleted<S: Into<String>>(resource_name: S) {
    let resource_name = resource_name.into();
    add_replay_event(ReplayEvent::TrChange { resource_name, change_type: TrChangeType::Delete });
}

pub fn resource_unmodified<S: Into<String>>(resource_name: S) {
    let resource_name = resource_name.into();
    add_replay_event(ReplayEvent::TrChange { resource_name, change_type: TrChangeType::Unmodified });
}

pub fn start_replay_log() {
    let grp = &*GLOBAL_REPLAY;
    grp.send_message(ReplayMsg::StartReplayLog);
}

pub fn set_replay_out_file(fp: PathBuf) {
    let grp = &*GLOBAL_REPLAY;
    grp.send_message(ReplayMsg::ReplayFile(fp));
}

pub fn finish_replay() {
    let grp = &*GLOBAL_REPLAY;
    grp.send_message(ReplayMsg::Finish);
}

pub async fn get_replay_copy() -> Result<Vec<ReplayLog>, String> {
    let (tx, rx) = tokio::sync::oneshot::channel();
    let grp = &*GLOBAL_REPLAY;
    grp.send_message(ReplayMsg::GetReplayCopy(tx));
    rx.await.map_err(|e| format!("failed to receive replay copy: {:?}", e))
}

pub fn set_replay_out_buffer<B: Write + 'static + Send>(buf: B) {
    let boxed_buf = Box::new(buf);
    let grp = &*GLOBAL_REPLAY;
    grp.send_message(ReplayMsg::ReplayBuffer(boxed_buf));
}

#[cfg(test)]
mod test {
    use std::{io, sync::{Arc, Mutex}, time::Duration};

    use super::*;
    use assert_matches::assert_matches;

    #[tokio::test]
    async fn replay_simply_dropped_until_replay_start_msg_sent() {
        let rp = create_replay_handle();
        rp.send_message(ReplayMsg::AddEvent(ReplayLog { t_ms: 1, d: ReplayEvent::ResourceRead {
            resource_name: "a".to_string(),
            value: serde_json::Value::Null,
        }}));
        let (tx, rx) = tokio::sync::oneshot::channel();
        rp.send_message(ReplayMsg::GetReplayCopy(tx));
        // it should error because replay hasnt started yet, so the tx was just dropped
        rx.await.expect_err("it should err");
        // now replay is started
        rp.send_message(ReplayMsg::StartReplayLog);
        rp.send_message(ReplayMsg::AddEvent(ReplayLog { t_ms: 1, d: ReplayEvent::ResourceRead {
            resource_name: "b".to_string(),
            value: serde_json::Value::Null,
        }}));
        // and now we can get results, which only has the latest resource read "b"
        let (tx, rx) = tokio::sync::oneshot::channel();
        rp.send_message(ReplayMsg::GetReplayCopy(tx));
        let logs = rx.await.expect("it should not err");
        assert_eq!(logs.len(), 1);
        assert_matches!(&logs[0].d, ReplayEvent::ResourceRead { resource_name, .. } => {
            assert_eq!(resource_name, "b");
        });
    }

    #[test]
    fn replay_can_write_to_file_and_flush_every_100_logs() {
        let tmp_file = PathBuf::from("/tmp/tmpfile1.jsonl");
        let rp = create_replay_handle();
        rp.send_message(ReplayMsg::StartReplayLog);
        rp.send_message(ReplayMsg::ReplayFile(tmp_file.clone()));
        for _ in 0..99 {
            rp.send_message(ReplayMsg::AddEvent(ReplayLog { t_ms: 1, d: ReplayEvent::ResourceRead {
                resource_name: "a".to_string(),
                value: serde_json::Value::Null,
            }}));
        }
        // reading the file now it doesnt have anything yet:
        std::thread::sleep(Duration::from_millis(200));
        let data = std::fs::read_to_string(&tmp_file).expect("it shouldnt fail");
        assert_eq!(data.lines().count(), 0);
        // write one more log, now it should flush:
        rp.send_message(ReplayMsg::AddEvent(ReplayLog { t_ms: 1, d: ReplayEvent::ResourceRead {
            resource_name: "a".to_string(),
            value: serde_json::Value::Null,
        }}));
        // wait some time to ensure the file is written to
        std::thread::sleep(Duration::from_millis(200));
        let data = std::fs::read_to_string(&tmp_file).expect("it shouldnt fail");
        assert_eq!(data.lines().count(), 100);
    }

    #[test]
    fn replay_can_write_to_arbitrary_buffer_instead_of_file() {
        struct MutexBuffer {
            pub data: Arc<Mutex<Vec<u8>>>,
        }
        impl Write for MutexBuffer {
            fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
                if let Ok(mut g) = self.data.lock() {
                    let len = buf.len();
                    g.extend_from_slice(buf);
                    return Ok(len)
                }
                Ok(0)
            }
        
            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }
        let mutex_buf = MutexBuffer {
            data: Arc::new(Mutex::new(vec![]))
        };
        let data_clone = mutex_buf.data.clone();
        let buffer_box = Box::new(mutex_buf);
        let tmp_file = PathBuf::from("/tmp/tmpfile2.jsonl");
        let rp = create_replay_handle();
        rp.send_message(ReplayMsg::StartReplayLog);
        rp.send_message(ReplayMsg::ReplayBuffer(buffer_box));
        // setting a replay file after setting the replay buffer is a no-op
        // nothing should go to this file:
        rp.send_message(ReplayMsg::ReplayFile(tmp_file.clone()));
        for _ in 0..100 {
            rp.send_message(ReplayMsg::AddEvent(ReplayLog { t_ms: 1, d: ReplayEvent::ResourceRead {
                resource_name: "a".to_string(),
                value: serde_json::Value::Null,
            }}));
        }
        // wait for buffer to be written
        std::thread::sleep(Duration::from_millis(200));
        // the buffer has 100 items
        // the file has nothing
        std::fs::read_to_string(tmp_file).expect_err("it shouldnt exist");
        let data_bytes = data_clone.lock().unwrap();
        let data_bytes = String::from_utf8(data_bytes.to_vec()).unwrap();
        assert_eq!(data_bytes.lines().count(), 100);
    }

    #[test]
    fn replay_can_be_finished() {
        let tmp_file = PathBuf::from("/tmp/tmpfile3.jsonl");
        let rp = create_replay_handle();
        rp.send_message(ReplayMsg::StartReplayLog);
        rp.send_message(ReplayMsg::ReplayFile(tmp_file.clone()));
        // we only write 20 items, didnt reach flush size yet
        for _ in 0..20 {
            rp.send_message(ReplayMsg::AddEvent(ReplayLog { t_ms: 1, d: ReplayEvent::ResourceRead {
                resource_name: "a".to_string(),
                value: serde_json::Value::Null,
            }}));
        }
        // reading the file now it doesnt have anything yet:
        std::thread::sleep(Duration::from_millis(200));
        let data = std::fs::read_to_string(&tmp_file).expect("it shouldnt fail");
        assert_eq!(data.lines().count(), 0);
        // now we finish the replay
        rp.send_message(ReplayMsg::Finish);
        // now the file should have 20 lines:
        std::thread::sleep(Duration::from_millis(200));
        let data = std::fs::read_to_string(&tmp_file).expect("it shouldnt fail");
        assert_eq!(data.lines().count(), 20);
    }

    #[tokio::test]
    async fn replay_collects_in_memory_until_file_or_buf_provided() {
        let tmp_file = PathBuf::from("/tmp/tmpfile4.jsonl");
        // ensure file doesnt exist
        let _ =std::fs::remove_file(&tmp_file);
        let rp = create_replay_handle();
        rp.send_message(ReplayMsg::StartReplayLog);
        // we dont send a file yet, so all the logs we send are in memory
        for _ in 0..20000 {
            rp.send_message(ReplayMsg::AddEvent(ReplayLog { t_ms: 1, d: ReplayEvent::ResourceRead {
                resource_name: "a".to_string(),
                value: serde_json::Value::Null,
            }}));
        }
        // reading the file now it doesnt have anything yet:
        std::thread::sleep(Duration::from_millis(200));
        std::fs::read_to_string(&tmp_file).expect_err("it should fail to read because file doesnt exist");
        // we can request the buffer copy, it should have 20,000
        let (tx, rx) = tokio::sync::oneshot::channel();
        rp.send_message(ReplayMsg::GetReplayCopy(tx));
        let logs = rx.await.expect("it shouldnt err");
        assert_eq!(logs.len(), 20000);
        // now we provide a file buffer:
        rp.send_message(ReplayMsg::ReplayFile(tmp_file.clone()));
        // if we request the logs again, it should have been flushed to the file:
        let (tx, rx) = tokio::sync::oneshot::channel();
        rp.send_message(ReplayMsg::GetReplayCopy(tx));
        let logs = rx.await.expect("it shouldnt err");
        assert_eq!(logs.len(), 0);
        // the file should have 20,000 lines:
        let file_data = std::fs::read_to_string(tmp_file).expect("it shouldnt error");
        assert_eq!(file_data.lines().count(), 20000);
    }
}
