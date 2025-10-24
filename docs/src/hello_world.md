# Hello World

Before we get into technical details about deploy-lang, let's look at a simple hello world example to get started.

Create a file called `hello_world.dpl` anywhere you'd like, and copy paste the following into your file:

```dpl
state
   file mystate.json
```

Now let's run our program using the `deploy-lang` cli. Simply run:

```bash
deploy-lang ./hello_world.dpl
```

After running this, you should see that a file was created in your current directory called `mystate.json`.

If you run `cat mystate.json` you can print the contents of this file, which should look something like this:

```json
{
  "dpl_metadata": null,
  "resources": {}
}
```

As you may have guessed, the `mystate.json` file contains the entire state of your application. An application's state is composed of `resource`s, which currently is an empty set! We'll learn more about state and resources in a later chapter, but let's wrap up our hello world example by actually creating a resource.

Edit your `hello_world.dpl` file and add the following after your `state` section:

```dpl
template echo
  create
    echo "hello world!"

resource echo(my_first_resource)
  {}
```

Your full `hello_world.dpl` file should now look like this:

```dpl
state
  file mystate.json

template echo
  create
    echo "hello world!"

resource echo(my_first_resource)
  {}
```

We added 2 new sections to the `hello_world.dpl` file: a `template` section that is named `echo`, and a `resource` section that **calls** the template `echo` and is named `my_first_resource`.


Now run the `deploy-lang` cli again:

```bash
deploy-lang ./hello_world.dpl
```

You should see some CLI output that contains

```text
resource 'my_first_resource' OK
```

Now examine the `mystate.json` file again and you should see it contains the following:

```json
{
  "dpl_metadata": null,
  "resources": {
    "my_first_resource": {
      "resource_name": "my_first_resource",
      "template_name": "echo",
      "last_input": {},
      "output": "hello world!",
      "depends_on": []
    }
  }
}
```

Here we can see that our state file now has 1 resource: the resource "my_first_resource" that we wrote in the `hello_world.dpl` file. This resource also has some extra information such as what template it called, its input and its output.

We could wrap up the hello world example here, but let's drive home the point of what deploy-lang is all about: deploy-lang is about managing resources over time. Run the `deploy-lang` command again:

```bash
deploy-lang ./hello_world.dpl
```

What happened? nothing! You should not have seen the output `resource 'my_first_resource' OK`. Why not? because *nothing changed* from the last time you ran `deploy-lang`. Your resource did not need to be **updated** because the inputs did not change. Remember in the `mystate.json` file we saw that it contained `"last_input": {}`? That's the part of the state that is compared against your resource's current input (the body of the `my_first_resource` resource in your `hello_world.dpl` file).

Edit your `hello_world.dpl` file and change the resource's body to something else, for example change it to this:

```dpl
# rest of the .dpl file omitted for brevity

resource echo(my_first_resource)
  { "a": "b" }
```

Then run the `deploy-lang` command again:

```bash
deploy-lang ./hello_world.dpl
```

You should see that it errors! It should have printed out an error message that contains something like

```text
resource 'my_first_resource' is to be updated but template 'echo' does not define any update commands
```

You updated your resource, and ran the `deploy-lang` cli again. deploy-lang saw that your resource's current input differs from its last input, and therefore it needs to be updated. but the template that it calls does not support updates! We can see from the template that it only has a subsection called `create` which will run `echo "hello world"!`.

In later chapters we'll learn more about templates, resource lifecycles and more.
