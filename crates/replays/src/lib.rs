use std::sync::{mpsc::Sender, LazyLock};

pub struct ReplayHandle {
    tx: Sender<ReplayMsg>,
}

enum ReplayMsg {
    AddEvent(ReplayEvent),
}

pub enum ReplayEvent {}

pub fn create_replay_handle() -> ReplayHandle {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        while let Ok(_msg) = rx.recv() {
            // TODO: do something with the message
        }
    });
    ReplayHandle { tx }
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

pub fn add_replay_event(event: ReplayEvent) {
    let grp = &*GLOBAL_REPLAY;
    grp.send_message(ReplayMsg::AddEvent(event));
}
