f<sup>3</sup>
=============

The Fantastic File Finder, for emacs.

Find files fast, using helm. It's cool, trust me.

# Goals

1. find file in project
    - also find file if it's an open buffer
2. run `find` with args
3. select multiple files and perform actions on them
4. do it fast
    - what does "fast" mean? who knows?

# Ideas
## use helm as a frontend
- basic command just finds files in project based on name
    - `find . -name "*a*" -and -name "*b*"` for `helm-pattern` = `a b`
- bust out keybindings for set intersections and then running commands
- these keybindings interpret the current pattern as an "and" pattern as shown above and then clear `helm-pattern` to eventually build a `find` command line
    - combinators: these set `f3-current-combinator`, take the current pattern as word, and reset helm
        - <kbd>M-+</kbd> = union ("or")
        - <kbd>M-*</kbd> = intersection ("and")
    - groupings: these call `f3-(open/close)-paren` and reset helm
        - <kbd>M-(</kbd> = open paren (will implicitly close any left at end)
        - <kbd>M-) M-+</kbd> = close paren / or (left parens will be implicitly added if needed)
        - <kbd>M-) M-*</kbd> = close paren / and
    - modes: these do NOT reset helm, just change `f3-current-mode`
        - <kbd>M-t</kbd> = normal text mode (not regex mode)
        - <kbd>M-x</kbd> = regex mode
        - <kbd>M-r</kbd> = "find" mode (just input raw find arguments)
        - <kbd>M-f</kbd> = filetype (`b|c|d|f|l|p|s`)
            - explode if not recognized
        - <kbd>M-p</kbd> = perm
    - modifiers: these change `f3-current-complement` and do NOT reset helm
        - <kbd>M-q</kbd> = toggle complement (current)
    - actions
        - <kbd>M-g</kbd> = run command on results in new `*Async Shell Command*` buffer and close helm
        - <kbd>M-d</kbd> = exit helm and list the files in a `find-dired` buffer
        - <kbd>M-l</kbd> = run lisp on results
        - <kbd>RET</kbd> = visit
        - <kbd>TAB</kbd> = preview
        - <kbd>M-b</kbd> = bounce to raw
    - meta
        - <kbd>M-u</kbd> = undo whatever was just done
            - also usable to edit atoms earlier (further left) in the stack and then return right
        - <kbd>M-U</kbd> = redo
        - <kbd>M-<</kbd> = set `mindepth`
        - <kbd>M-></kbd> = set `maxdepth`

## other helm things in general

add these to all helm searches

- <kbd>M-o</kbd> = start search from project root
- <kbd>M-i</kbd> = start search from initial choice of `default-directory`
- <kbd>M-c</kbd> = choose directory to search from, starting at whatever the current choice is
- <kbd>M-j</kbd> = start searching a directory up


# License

[GPL](GPL.md)
