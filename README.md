f<sup>3</sup>
=============

The Fantastic File Finder, for emacs.

Find files fast, using helm. It'll be cool, trust me.

# Goals

1. find file in project
    - also find file if it's an open buffer
2. run `find` with args
3. select multiple files and perform actions on them
4. do it fast

# Ideas
## use helm as a frontend
- basic command just finds files in project based on name
    - `find . \( -name "*a*" \) -and \( -name "*b*" \)` for `helm-pattern` = `a b`
- bust out keybindings for set intersections and then running commands
- these keybindings interpret the current pattern as an "and" pattern as shown above and then clear `helm-pattern` to eventually build a `find` command line
    - <kbd>M-u</kbd> = union
    - <kbd>M-i</kbd> = intersection
    - <kbd>M-c</kbd> = complement (current)
    - <kbd>M-r</kbd> = regex mode
    - <kbd>M-a</kbd> = "arg" mode (raw find arguments)
    - <kbd>M-(</kbd> = open paren (will implicitly close any left at end)
    - <kbd>M-)</kbd> = close paren
    - <kbd>M-g</kbd> = run command on results in new `*Async Shell Command*` buffer and close helm
    - <kbd>M-f</kbd> = prompt for filetype (`b|c|d|f|l|p|s`)
    - <kbd>M-p</kbd> = start search from project root
    - <kbd>M-d</kbd> = start search from initial choice of  `default-directory`
    - <kbd>M-f</kbd> = choose directory to search from, starting at whatever the current choice is
    - <kbd>M-j</kbd> = start searching a directory up
- otherwise, pressing <kbd>RET</kbd> on a result visits it, and <kbd>TAB</kbd> previews it

# License

[GPL](GPL.md)
