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
  make-key                 make a secret key
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

  
```bash
km generate --help
Usage: km generate <LENGTH> --username <USERNAME> [-u|--upper-case]
                   [-l|--lower-case] [-n|--number] [-s|--special]
                   (-d|--desc <DESCRIPTION>)

  generate a new key

Available options:
  <LENGTH>                 length of the password
  --username <USERNAME>    username
  -u,--upper-case          with upper case required
  -l,--lower-case          with lower case required
  -n,--number              with number required
  -s,--special             with special char required
  -d,--desc <DESCRIPTION>  description
  -h,--help                Show this help text
```
