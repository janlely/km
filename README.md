# km
a key manage tool

```bash
km --help
km - a key manage tool

Usage: km COMMAND

  Key Manager

Available options:
  -h,--help                Show this help text

Available commands:
  add                      input a new key
  query                    query key
  get                      get key by id
  generate                 generate a new key
  list                     list all keys
  del                      delete key by id
```

```bash
km add --help
Usage: km add (-u|--username <USERNAME>) (-p|--password <PASSWORD>)
              (-d|--description <DESCRIPTION>)

  input a new key

Available options:
  -u,--username <USERNAME> username
  -p,--password <PASSWORD> password
  -d,--description <DESCRIPTION>
                           key description
  -h,--help                Show this help text
```

```bash
km query --help
Usage: km query <KEY WORDS>

  query key

Available options:
  -h,--help                Show this help text
```
```bash
km get --help
Usage: km get <id>

  get key by id

Available options:
  -h,--help                Show this help text
```

```bash
Usage: km del [<id>]

  delete key by id

Available options:
  -h,--help                Show this help text
```
