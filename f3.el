;;; make an async source of sorts such that when the query is changed:
;;; 1. the current set of results which match the new query are fed into the
;;; newly-spawned find process so that they're not duplicated, and are already
;;; available to search for the next query, and
;;; 2. if results in the previous query match the new query, they are kept in
;;; the buffer (thanks to 1., i think), and actions (such as calling find-file)
;;; can be performed on them

;;; consider writing find replacement which can run faster, with a more limited
;;; set of options. it's unlikely this will work since find is in c and i don't
;;; want to have to package and maintain a c executable over a million different
;;; distributions, but it is possible

;;; make the pattern, instead of just a regexp, allow for different find options

;;; think about cool actions you can take on the input. previewing? getting file
;;; stats? running an executable? what options does helm have to union /
;;; intersection / complement search results? can we integrate icicles?

;;; when this works, make the source creation part into an external library, and
;;; then depend on that

;;; don't forget to read the find man page about optimization levels

(provide 'f3)
