# Concepts

A deploy-lang file (`.dpl` file) contains several root level concepts to be aware of that will help form a mental model of how deploy-lang works.

## Comments

A `.dpl` file uses the `#` character for comments. Multiline comments are not supported. The `#` character can appear above/around sections, or at the end of a line. Certain sections such as `resource` sections even support comments in its body, for example consider this sample `.dpl` file:

```dpl
# comment above a resource
resource some_template(my_resource_name) # comment on the same line
  {
    # you can have comments here!
    "a": {
      # and here!
      "b": "c" # and here!
    }
  }
```

## Sections

A `.dpl` file is composed of sections, each section potentially written in a different syntax/language. A section starts with a keyword describing the section type. common section types are `resource`, `template`, `state`. Following the section type, on the same line, are optional `parameters` to the section.

After the section type and optional parameters comes the section body. DPL is indentation based, so the body must have at least 1 character of whitespace (either a tab or a space). The section body contains all lines with at least 1 whitespace character until an empty line is found (or the end of the document), in other words a section must end with `\n\n`. Let's look at a full example of a `resource` section:


```dpl
#                 /---  parameters ---\
#                |                     |
# section type   |                     |
#   |           /                      |
#   |          /                       |
#   |         /                        |
#   |        /                         |
#   |       /                          |
#  /       /                           |
# |       /                            |
# v      v                             v
resource some_template(my_resource_name)
  {                                      #  body starts here
    "hello": "world"                     #  body
  }                                      #  body ends here
```

The section type is `resource`, the parameters are `some_template(my_resource_name)` and the body includes three lines of json:
```dpl
  {
    "hello": "world"
  }
```

## File

deploy-lang is written in files with extension `.dpl`, from here forward we may refer to deploy-lang, `.dpl` files, and DPL files interchangeably. Above we showed an example of a single section in a DPL file, but usually a DPL file will be composed of several different sections, each section potentially having a different syntax, and performing a different task. Let's look at a simple DPL file that has a variety of section types:

```dpl
state
   file mystate.json

template hello
  create
    echo "hello world!"

resource hello(my_first_resource)
  {}
```

In this example, we have 3 sections, a `state` section, a `template`, and a `resource` section.

1. The `state` section defines where deploy-lang will read and write your state. The `state` section does not have any parameters. This section's body simply has `file mystate.json`, which tells deploy-lang that state will be managed simply via a file on disk, and that file is called `mystate.json`. In a later chapter we will explore state more in-depth and see different types of state that deploy-lang can read/write to/from.
2. After the `state` section we have a `template` section. This section does have a parameter and the parameter is `hello`. Template sections must have a parameter which is interpreted to be the name of the template. The body of the template simply says "on creation, run an "echo" command to echo "hello world!". Templates can be fairly complex, and in a later chapter we will learn more about templates, but for now just know that we have defined a template called `hello` that can print out "hello world!"
3. Finally, we have a `resource` section, this also must have a parameter, which is interpreted to be the template that the resource "calls" and in parentheses is the name of the resource. The resource section must have a body, which is JSON, and in this case it's just an empty JSON object.
