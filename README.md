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
    - combinators: these set `f3-current-combinator`, take the current pattern as word, and reset helm
        - <kbd>M-o</kbd> = union ("or")
        - <kbd>M-a</kbd> = intersection ("and")
    - groupings: these call `f3-(open/close)-paren` and reset helm
        - <kbd>M-(</kbd> = open paren (will implicitly close any left at end)
        - <kbd>M-)</kbd> = close paren
    - modes: these do NOT reset helm, just change `f3-current-mode`
        - <kbd>M-t</kbd> = normal text mode (not regex mode)
        - <kbd>M-r</kbd> = regex mode
        - <kbd>M-f</kbd> = "find" mode (just input raw find arguments)
        - <kbd>M-y</kbd> = filetype (`b|c|d|f|l|p|s`)
            - explode if not recognized
    - modifiers: these change `f3-current-complement` and do NOT reset helm
        - <kbd>M-c</kbd> = toggle complement (current)
    - actions
        - <kbd>M-g</kbd> = run command on results in new `*Async Shell Command*` buffer and close helm
        - <kbd>M-l</kbd> = list the files in a dired buffer and exit
        - <kbd>RET</kbd> = visit
        - <kbd>TAB</kbd> = preview
    - meta
        - <kbd>M-z</kbd> = undo whatever was just done
        - <kbd>C-M-z</kbd> = redo
        - <kbd>M-m</kbd> = set `mindepth`
        - <kbd>M-d</kbd> = set `maxdepth`

## other helm things in general

add these to all helm searches

- <kbd>M-p</kbd> = start search from project root
- <kbd>M-i</kbd> = start search from initial choice of `default-directory`
- <kbd>M-c</kbd> = choose directory to search from, starting at whatever the current choice is
- <kbd>M-j</kbd> = start searching a directory up


# License

[GPL](GPL.md)
