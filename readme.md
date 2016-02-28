I've just found [another quick scope package] [1] for evil mode. It was created
before this package, and it's much better written, and also has an extensive
test suite, so you should definitely try that one out first.

I wrote this because I wasn't aware of any others at the time I wrote it. It's
possible blorbx's package was not public yet when I was searching if one already
existed.

For the time being, I will continue using and working on this package because
there's a few features that are not in blorbx's version that I want. However,
when/if these things come to blorbx's quick scope, I see no reason to continue
this package and will deprecate it. So be cautious if you use this package, and
keep checking back periodically, in case it has been deprecated.

With regards to the function prefixes, blorbx's code seems to use
`evil-quickscope` as a prefix throughout, whereas I use `evil-qs`, except I also
call the mode `evil-quick-scope-mode` and the group for the faces
`evil-quick-scope`. So there shouldn't be conflicts, but you should only be
using one or the other anyway.

[1]: https://github.com/blorbx/evil-quickscope
