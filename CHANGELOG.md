Darcs 2.18.3, 26 May 2024

  * relax upper bounds for some dependencies

  * with GHC up to 9.8 all tests now succeed (including Windows and MacOS)

  * resolve issue2714: cannot remove recursively

  * resolve issue2715: hub.darcs.net does not support "Extended Main Secret"

    We use a new addition to crypton-connection in order to change the default
    setting back to old (tls < 2.0) default.

  * resolve issue2721 by excluding certain versions of directory package

    Versions 1.3.8 up to 1.3.8.4 of the directory package have a bug in their
    implementation of copyFile on Windows.

  * work around issue2720 (caused by x509-system indirect dependency, see
    (see https://github.com/kazu-yamamoto/crypton-certificate/issues/9)

  * make execution of release/gen-version-info.hs more robust

    Using cabal run instead of runghc inside of Setup.hs ensures that we use the
    same ghc version that cabal uses, avoiding dependency errors.

Darcs 2.18.2, 24 Mar 2024

  * Fix deprecated head/tail warnings on GHC 9.8, making the build there
    warning-free.

  * Replace the set-default hint that was removed in 2.18.1

  * Add a --ghcflags/-g option to the test suite to allow flags
    like -dynamic to be passed to GHC when it builds helper exes
    in various test scripts.

Darcs 2.18.1, 25 Feb 2024

  * Supports GHC 9.8 and the most recent version of other dependencies
    at the time of release, with the exception of the tls 2.0 package,
    which has been held back because of problems connecting to hub.darcs.net
    (see https://bugs.darcs.net/issue2715).

  * Substantial rewrite of the 'darcs test' command.

    The most important user visible change is that a test script can now
    return an exit code of 125 to reflect an untestable/skipped state
    (as with with "git bisect run").

    This in turn means that a group of patches can be found to be responsible
    for a failure rather than just a single one. By default, Darcs will
    try to minimise such a group by reordering patches to remove irrelevant
    ones from the initial group found from the patch ordering in the
    repository. This behaviour can be disabled with --no-shrink-failure.

  * Remove support for downloading via curl

    This is no longer particularly useful as we now use modern, maintained
    Haskell libraries for native HTTP downloading, and substantially simplifies
    this area of the code.

  * Patch index: Significant performance improvement

    The patch index is used in commands like annotate and log. A couple of
    performance improvements were made that should speed up using the patch
    index.

  * Progress reporting

    Progress reports are now provided during more long-running operations,
    including updating the "index" (a cache that speeds up detecting
    changes in the working directory), and during merge operations.

    They also behave better on Windows and when outputting long lines.

  * Other changes/fixes:
    * Use hardlinks more often to share files between repositories/caches.
    * Support --leave-test-dir for all commands that support --test
    * Avoid extraneous "repo:." entries in _darcs/prefs/sources [issue2672]
    * Add 'darcs clean' command as an alias for 'darcs revert -l'
    * 'darcs rebase unsuspend': add more patch editing options
    * Fix stale lock files after Ctrl-C
    * External merge tools: preserve output, and fail if tool does
    * Properly reference renamed files in external merge [issue189]
    * Mark conflicts properly if tag pulled at the same time [issue2682]
    * Remove the useless optimize pristine subcommand
    * 'darcs convert': honour the --compress and --diff-algorithm options
    * Fix contrib/darcs-shell [issue2646]
    * Fix 'darcs pull --dont-allow-conflicts' with external-merge [issue1819]
    * Problems with local pristine files now tell user to run
      'darcs repair' [issue1981]
    * Fix various problems with symlinks, including on Windows
    * Add --no-prefs-templates option when creating a repository
    * Allow 'darcs rebase unsuspend' when there are non-conflicting
      unrecorded changes
    * Handle broken pending patches in 'darcs check' and 'darcs repair'
    * Improve error reporting when remote _darcs/format doesn't exist
    * 'darcs optimize reorder': add --deep/--shallow options
    * 'darcs optimize compress/uncompress': also handle pristine files
    * 'darcs optimize cache': don't work with lists of darcs repos
      Instead just the global cache is cleaned by checking hard-link counts
    * Skip the pager when $DARCS_PAGER / $PAGER are set to the empty string
    * 'darcs convert export': allow relative paths for --read-marks and
      --write-marks
    * Fix 'darcs amend --unrecord' to move unrecorded changes to pending
      [issue2697]
    * Don't treat cancelling an operation as failure [issue2074]
    * Fix cloning of ssh repo when using Ctrl-C to stop getting patches
      [issue2701]
    * Don't report invalid regexes as a bug in darcs [issue2702]
    * Add short option -n for --dry-run
    * 'darcs diff': support --look-for-moves and --look-for-adds
    * Fix buffering problem with 'darcs diff' [issue2704]
    * 'darcs obliterate' and 'darcs rebase': offer to revert any conflicting
      unrecorded changes
    * Stop displaying context lines in various interactive scenarios:
      it didn't work properly and would require a lot of work to fix.
    * Improve conflict display for Rebase and V3 patches
    * Increase size limit to 100K for environment variables like
      DARCS_PATCHES_XML, and warn when it is exceeded
    * 'darcs rebase unsuspend': improve the display of dropped dependencies
    * 'darcs amend --ask-deps': also provide a way to remove dependencies
    * 'darcs push': support --reorder-patches option
    * Remove the --remote-repo option
    * Don't display hints about using --set-default
    * Conflict resolution: unrecorded changes will suppress conflict marking
      of appropriate changes [issue2708]
    * Explain what a "clean tag" is in help for tag command
    * Fix problem with naming patch bundles after patches that contain
      characters incompatible with the current locale [issue2716]

Darcs 2.16.5, 20 Feb 2022

  This release is to support newer dependencies, most importantly
  GHC 9.0 and Cabal 3.6.

  It also includes a small number of refactors that help with moving
  to those newer dependencies.

Darcs 2.16.4, 20 May 2021

  This release is mostly to fix http://bugs.darcs.net/issue2674 which can
  lead to repository corruption. This is not quite as bad as it sounds,
  since the broken changes that you could have recorded were consistently
  ignored when applying the patch. This bug has been in darcs for a very
  long time and even our own repos contain (ancient) patches with broken
  move changes, and so far it hasn't caused us any trouble.

  That said, there are certain patch commutations that will erroneously (and
  unexpectedly) fail when such a patch is involved. We therefore recommend
  to upgrade. You may also (after upgrading) run 'darcs check' on your
  repositories to see if you are affected, and 'darcs repair' them if that
  is the case. Fortunately, the broken move changes can be safely eliminated
  from existing patches, and the improved repair command now does exactly
  that.

  Thanks to lemming@henning-thielemann.de for bringing this bug to our
  attention.

  * resolve issue2674: moving unadded files
  * add a check/repair rule for bad move patches
    This drops a move patch with either non-existing source or existing target.

  Also related to issue2674:

  * improve error message when runDefault fails due to an IO error
  * remove catching of exceptions for bad moves in DefaultIO
  * fail in Darcs.Util.Tree.Monad.rename if source does not exist
  * test that we cannot record patches that depend on broken moves

  The rest of the changes are minor bug fixes plus a few dependencies.

  * resolve issue2670: convert "." to absolute path when creating a repo
    This bug was introduced by fix for issue2668.
  * resolve issue2668: createDirectory: permission denied
  * resolve issue2667: darcs init failed with permission denied
  * bugfix: --leave-test-dir should be off by default
  * bash_completion: use '--list-options' before any other option
    This is so to avoid that, e.g.:
    $ darcs record -m <Tab><Tab>
    results in a patch named '--list-options' being recorded.
  * zsh completion: use '--list-options' before any other option
    See the corresponding change in the bash completion for details.
  * zsh completion: improve the get/clone case
    Moving the special case for get/clone down into the catch-all case for the
    current word allows it to complete options and local paths.
  * bugfix: --leave-test-dir should be off by default
  * bugfix in readPendingAndMovesAndUnrecorded
  * print patch application warnings to stderr, not stdout

Darcs 2.16.3, 22 October 2020

  * Fix building with `-f curl` (issue2655)
  * Fix building with stack
  * Various fixes in our custom Setup.hs, mostly to do with cabal commands
    exectuted inside the unpacked source dist tar ball
  * Remove obsolete dependency on split package
  * Remove dependency on sandi and use base16-bytestring >= 1.0, improving
    performance when darcs handles binary files and patches, and more
    generally whenever we convert hashes from/to text.
  * Various minor fixes and additions to tests scripts
  * Issues fixed:
    * 2654: amend --prompt-long-comment removes the long comment
    * 2658: show dependencies should only show direct dependencies
    * 2659: check for bad patch name after invoking editor, too

Darcs 2.16.2, 19 August 2020

  * Fix build problem when using 'cabal install' from inside the sdist.

    This fails because in this case cabal will try to 'cabal sdist' the
    bundled shelly dependency, and we do not (nor want to) list all shelly
    files in our own cabal file. To avoid problems like this, we no longer
    use the bundled shelly as a dependency, but rather as part of our
    sources for the test suite.

Darcs 2.16.1, 14 August 2020

  * Building:
    * Drop support for building with ghc-8.0
    * Allow clean (warning-free) builds with all ghc versions from 8.2.2 up
      to ghc-8.10.1
    * Various dependency updates
    * Remove sdist and postConf hooks from Setup.hs
    * move -DHAVE_MAPI from Setup.hs to darcs.cabal
    * Recommended way to build is using cabal-install >= 3.0
    * The source tree now contains a cleaned-up version of shelly-1.7.1
      (locally named shelly-1.7.1.1) which we need to run our test suite.
      Unfortunately, later versions break compatibility on Windows and
      keeping the dependency fixed to 1.7.1 would mean we cannot support
      newer ghc versions.

  * Preliminary UNSTABLE support for a new patch theory named "darcs-3",
    largely based on the pioneering work of Ian Lynagh for 'camp'.

    Please note that this format is not yet officially supported: some
    features (like conversion from older formats) are still missing, and we
    have not yet finalized the on-disk format. You should NOT use it for any
    serious work yet.

    The new theory finally solves all the well-known consistency problems
    that plagued the earlier ones, and thus fixes a number of issues
    (including issue1401 and issue2605) that have been outstanding for many
    years. It also reduces the worst case asymptotic runtime for commutation
    and merging from exponential to merely quadratic in the number of
    patches involved.

    One of the reasons we are confident this new theory and its
    implementation is sound, i.e. respect all required properties, is that
    we have improved our test case generator for sequences of patches. It
    now generates all possible conflict scenarios. Since the new theory no
    longer has worst case exponential runtime, we can and did test all
    required properties and invariants with a large number of generated test
    cases (up to 100000).

  * The internals of how 'darcs rebase' stores and handles suspended patches
    and their "fixups" has been changed in incompatible ways. This means
    that if you have a rebase in progress started with darcs < 2.16, you
    will first need to use the new 'darcs rebase upgrade' command to upgrade
    the suspended patches to the new format. If you start a rebase with
    darcs-2.16, then earlier darcs versions will not work with that repo,
    until you have finished the rebase.

    The new implementation fixes a lot of bugs (including outright crashes
    in some situations). The behavior when conflicted patches are suspended
    is much better now, though there are still a few corner cases where the
    behavior can be quite unintuitive, especially when complicated conflicts
    are suspended.

    A number of limitations regarding repositories with a rebase in progress
    have been lifted; in particular, push, pull, and clone between repos can
    now be done regardless of whether any of the repos have a rebase in
    progress or not.

  * The way conflict markup is generated has been cleaned up and refactored.
    The main user-visible improvement is that darcs now reliably /either/
    marks a conflict /or/ keeps the default resolution (i.e. remove both
    changes) and reports the conflicts that it cannot mark. Previously,
    conflicts that could not be properly marked (roughly all conflicts
    involving changes other than hunks, e.g. replaces and file or dir adds,
    removes, and renames) would be silently "half-resolved" in favour of one
    of the alternatives. This could be pretty confusing, the more so since
    it was hard to predict which of the conflicting alternatives was chosen.

  * Complete re-implementation of the way in which the pending patch is
    updated after record and amend. This fixes a number of problems with
    pending becoming corrupt. The new code also works in cases where hunks
    are edited interactively. The algorithm is documented in detail.

  * Downloading files via http now uses the http-conduit package. This is
    now the default method when building darcs. Building against the curl
    library is still supported but you have to explicitly request it by
    passing -fcurl to cabal.

  * Reworked internal failure handling so we can clearly distinguish between
    normal command failures and internal errors, i.e bugs, in darcs. In case
    of a bug, darcs exits with status 4 and prints a message that asks the
    user to report it and how to do that.

  * During interactive patch selection, the 'x', 'v', and 'p' keys no longer
    print the patch description (if any), only the summary or the patch
    content. A new key 'r' was added to re-display the currently selected
    patch in default mode (normally this is just the description). See
    issue2649 for details.

  * A large number of other internal refactors and code cleanups.

  * A forward compatibility bug (issue2650), roughly fixed in Darcs 2.14.5,
    is now resolved in full. In particular, we modify the format file only
    if we have taken the repo lock, silently fix possibly corrupted entries
    when reading the format file, internally document the forward
    compatibility rules, and finally test them more thoroughly.

  * Issues fixed:
    * 1316: amend-record: files/dirs still in pending even if they are removed
    * 1609: darcs conflict marking gives different results in different orders
    * 2001: repair fails to detect missing pristine files
    * 2275: ignore symlinks as repo paths even when the index is used
    * 2404: darcs convert export ignores --repodir
    * 2441: Use pager for darcs annotate
    * 2445: internal error if suspended patch is pulled into repository again
    * 2454: help markdown/manpage should use a pager
    * 2533: add umask option to all commands that modify the repo
    * 2536: show files --no-files: can't mix match and pending flags
    * 2548: inconsistent pending after addfile f; rm f; mkdir f
    * 2550: apply only properly mangled resolutions, warn about any others
    * 2592: update pending with coalesced look-for changes
    * 2593: network test can collide with shell tests
    * 2594: darcs show index crashes replace with unrecorded force hunk
    * 2599: don't bother to update pending when cloning a repo
    * 2603: warn and mark conflicts when cloning
    * 2604: remove --reply and related options
    * 2608: download _darcs/hashed_inventory separately
    * 2610: add --inherit-default option
    * 2614: (an intermediate regression)
    * 2618: option --ask-deps adds too many dependencies
    * 2625: catch only IO exceptions from applyToWorking
    * 2626: treat applyToWorking more uniformly
    * 2634: use unwind to suspend patches
    * 2635: build/install man page only if we build darcs executable
    * 2639: darcs diff crashes with --last=1 and file name
    * 2645: search for ":" to detect ssh URLs only up to the first "/"
    * 2648: convert import with non-ASCII meta data and filepaths
    * 2649: cleanup display of patches

  * Partial fixes:
    * 1959: read-only commands should not need write access to the index
      This is mostly fixed, see tests/issue1959-unwritable-darcsdir.sh
      for the few remaining problematic cases. We also now check
      writability of the index when we start a transaction.

  * Miscellaneous user-visible changes and bugfixes:
    * always prompt for confirmation when there are conflicts with unrecorded
      changes
    * darcs optimize upgrade: don't throw away pending
    * use cryptonite instead of cryptohash and random; the random junk added
      to patch meta data now uses a cryptographically secure random number
      generator; also replaces our own implementation of SHA1
    * darcs repair: handle broken binary patches
    * fail if pending patch cannot be parsed instead of silently ignoring it
    * darcs remove: don't allow removal of root
    * darcs suspend reify: give reified fixup patches a real author
    * darcs add: fail unconditionally when no files were added
    * darcs optimize compress: don't compress special patches such as
      pending or unrevert
    * darcs send: bugfix on Windows with GHC>=8.6
    * fix prompting when we get a bad patch name from the user
    * darcs apply: fix interpretation of patch bundles as patchsets when the
      context tag is not in our repo
    * darcs apply: fix lazy reading of inventories
    * darcs amend: fix editing of tag names
    * clone to ssh: don't overwrite existing remote target directories
    * darcs rebase: remove error if no suspended patches found
    * fully respect the (badly named) --no-ignore-times option (which
      actually means to ignore the index)
    * replace the code for creating unique temporary directory names
      (that was prone to race conditions); instead we now use the temporary
      package
    * darcs check/repair: detect and repair missing (hashed) pristine files
    * remove option --restrict-paths (is always active now)
    * remove defunct --set-default option for rebase pull
    * remove env var DARCS_DO_COLOR_LINES
    * demote errors in defaults file and commandline to warnings
    * add --not-in-remote option to amend and rebase suspend; the option
      is now supported by all history editing commands.
    * remove our own optimisation settings in darcs.cabal
    * regard explicit dependencies as resolving conflicts
    * suspend reify: give reified fixup patches a real author
    * never overwrite existing files with -O/--output-auto-name
    * make working with temporary directories more robust
    * make sure we cancel pending download actions at exit
    * darcs diff: allow use of interactive external diff commands
    * remove graphviz dependency by re-implementing show dependencies
    * darcs show dependencies: review matching options
    * darcs annotate: more than one file argument is now an error

  * Changes in the output of commands:
    * add a warning when the index code ignores a symlink
    * print remote execution failure message to stderr
    * darcs amend: respect verbosity options
    * commands that can produce large amounts of output now display it using
      a pager, similar to 'darcs log'
    * remove "withSignalsHandled:" from message when we are interrupted (Ctrl-C)
    * add progress reporting to patch index
    * print remote execution failure message to stderr
    * re-formulate the bad sources hint
    * remove hint "Do you have the right URI for the repository?"
    * remove the "darcs failed:" from error messages
    * re-formulate the set-default hint
    * darcs check/repair: no coloring in progress reports
    * add progress reporting when creating packs
    * darcs test: make linear search report results like bisect
    * colorize --dry-run output and warnings
    * darcs whatsnew: print via pager (unless --xml or -s is active)
    * darcs apply: print "reading from stdin" unless --quiet
    * darcs send: include all the information when reporting exceptions
    * print name of patch bundle for obliterate --output

  * Deliberate API changes to support darcsden:
    * export getPrefLines
    * export a simplified version of getLogInfo
    * export runWithHooks instead of recordConfig and RecordConfig

  * Documentation/help changes:
    * extend help text for darcs show and darcs convert
    * re-word some option descriptions
    * improve the (top level) usage text
    * update help for defaults file(s)
    * add subcommand 'darcs help preferences'
    * improve help for _darcs/prefs/sources
    * darcs diff: fix help for --unified option
    * fix manpage formatting of bullet lists
    * fix docs and description for the status command
    * automatically format (most) help texts to 80 chars per line
    * replace initial blurb in the manpage
    * unify warning hints for history editing commands
    * move debugging options to the end of the advanced options
    * shorten the help for alias commands
    * add extended help for rebase super command
    * include help for super commands in the man page
    * group --reorder under the merge options and improve its help text
    * bring help text for 'darcs repair' up to date
    * darcs.cabal: fix license and reword package description

Darcs 2.14.5, 6 August 2020

  * Resolve issue2650

    This is a stupid and rather unfortunate bug that affects all previous
    versions of the 2.12 and 2.14 branch. It can lead to corruption of the
    _darcs/format file in repos where *future* darcs versions add an
    alternative format property. The bug affects commands rebase suspend,
    rebase pull, and rebase apply.

    It is possible to manually fix the corruption by deleting from
    _darcs/format strings of the form "Unknown property: ". Future releases
    will contain a work-around that automatically fixes this particular kind
    of corruption when reading the format file, in case it should ever
    happen in practice. Nevertheless we strongly recommend to upgrade and
    avoid using the darcs versions affected by this bug.

Darcs 2.14.4, 28 April 2020

  * Restored the ability to run our shell tests, at least when building
    directly from a clone of our darcs repo. This was done by importing an
    old version of shelly (1.7.1, the last that worked for us on Windows),
    so that (modern) cabal picks that version instead of the newest one from
    hackage. Then made it build with all supported ghc versions and fixed
    all warnings.
  * Fix the quick-and-dirty "solutions" to the MonadFail incompatibility
    that replaced fail with error to avoid cascading MonadFail requirements
    all over the place. This broke a number of our tests, proving it to be
    semantically unsound, as I had expected. In most cases the correct
    solution was to replace it with (throw . userError), or if possible with
    (liftIO . fail).

Darcs 2.14.3, 24 April 2020

  * Support for GHC 8.8 and GHC 8.10
  * Loosen upper bounds for a few dependencies
  * mitigate issue 2643 (corrupt patch index) with a better error message
  * remove our own optimisation settings in darcs.cabal
  * Setup.hs: allow use of darcs as a cabal subproject

Darcs 2.14.2, 26 January 2019

  * Support GHC 8.6 (Ganesh Sittampalam)
  * Some other dependency bumps (Ganesh Sittampalam)
  * Fixed the following bugs:
    * 2617 convert import crashes with out-of-order tags (Ben Franksen)

Darcs 2.14.1, 24 June 2018

  * Some dependency bumps (Ganesh Sittampalam, Ben Franksen, Guillaume Hoffmann)
  * Windows test fixes (Ganesh)
  * Fixed the following bugs:
    * 2588 clone creates target repo with wrong permissions (Ben)

Darcs 2.14.0, 4 April 2018

  * fix encoding business, make DARCS_DONT_ESCAPE_8BIT=1 default (Ben, Ganesh Sittampalam)
  * show explicit dependencies in `darcs log -s` (Gian Piero Carrubba)
  * improve bash/zsh completion (Ben, Gian Piero)
  * no longer print an error message when ctrlc'ing pager (Guillaume Hoffmann)
  * `darcs help markdown` mentions all files in `_darcs/prefs/` (Guillaume)
  * add patch index status to `show repo` command (Ben)
  * per-file conflict marking (Ben Franksen)
  * make it possible to use DARCS_SCP=rsync (Ben)
  * add --not-in-remote option to unrecord command (Ben)
  * plug memory leak and improve efficiency in annotate (Ben)
  * save unneeded FL/RL reverses in SelectChanges module (Ben)
  * optimize token replace code and --look-for-replaces (Ben)
  * no longer show conflicting files on `whatsnew -s`, will reintrodue this
    feature when it is done efficiently (Guillaume)
  * separate display and storage of patches (Ben)
  * support GHC 8.2 and GHC 8.4 (Ganesh)
  * many refactorings in Darcs.Repository modules and API (Ben, Guillaume)
  * no longer track build dependencies in Setup.hs, nor use
    alpha, beta, rc names (Guillaume)
  * refactor `pull --reorder-patches` (Ben)
  * refactor SelectChanges (Ben)
  * remove Patchy typeclass and redundant constaints where possible (Guillaume)
  * fix build with cabal new-build (Francesco Ariis)
  * unit and quickcheck tests for inventories (Ben)
  * throw out all access to bytestring internals from Darcs.Util.ByteString (Ben)
  * refactor, simplify, and document hunk application (Ben)
  * drop support of old cache location and SHA1-hashed repos (Guillaume)
  * rely on GHC's own stack traces for bug reporting (Guillaume)
  * fixed the following bugs:
    * fix mail encoding with '.' or '=' as last character (Timo von Holtz)
    * 2526: whatsnew -l --boring should list boring files (Ben)
    * 2208: replace detects existing force hunks in working (Ben)
    * 2512: author name is written to repository after multiple-choice
      prompt (Stephan-A. Posselt)
    * 2359: convert --export mishandles Unicode filenames (Ben)
    * 2545: prevent argument smuggling in SSH repository URLs (Gian Piero)
    * 2581: fix rebase pull --reorder (Ben)
    * 2575: fix unrevert with rebase (Ben)
    * 2579: allow darcs send to work even if no MTA is installed
    * 2555: include explicit dependencies in the output of `log -v` (Gian Piero)
    * 2569: decoding multibyte characters (Ben)
    * 2563: create remote repo in correct format in ssh tests (Ben)
    * 2565: create _darcs dir after searching for an existing one (Ben)
    * 2567: darcs whatsnew --unified (Ben)
    * 2566: avoid renaming across file systems (Ben)
    * 2564: delete wrong and irrelevant propConcatPS (Guillaume)
    * 2559: remove trailing empty lines in patch header edition (Guillaume)
    * 2536: mask out internal matchers in `show files` routing logic (Gian Piero)


Darcs 2.12.5, 11 January 2017

 * Bump some dependency upper bounds (Ganesh Sittampalam)
 * Fix issue2516 - failure cloning from URLs on Windows (Ben Franksen)


Darcs 2.12.4, 14 September 2016

 * *really* fix compile error under Windows (Guillaume Hoffmann)


Darcs 2.12.3, 10 September 2016

 * fix compile error under Windows (Guillaume Hoffmann)


Darcs 2.12.2, 7 September 2016

 * fix missing testsuite file in tarball (Guillaume Hoffmann)


Darcs 2.12.1, 5 September 2016

 * fix building with GHC 8
 * drop support for GHC 7.6 and 7.8, i.e., require GHC 7.10
 * improvements in `darcs whatsnew` output with irrelevant files (Ben Franksen)


Darcs 2.12.0, 29 April 2016

 * `darcs show dependencies`: export patch dependency graph as dot file (Ale Gadea)
 * improvements in `record` output with irrelevant files (Ben Franksen)
 * `darcs log -v --machine-readable`: show internal representation of
   patches (including explicit dependencies). Remove patch viewing via the
   `annotate` command. (Guillaume Hoffmann)
 * `whatsnew -s` (and `status`) show conflicting files (Guillaume Hoffmann)
 * honor "quiet" flag in command outputs (Ben Franksen)
 * a single `show patch-index` command (Guillaume Hoffmann)
 * remove deprecated aliases of show (Guillaume Hoffmann)
 * handle file moves natively when importing from git (Owen Stephens)
 * require GHC 7.6 (base > 4.6) and support GHC 8 (Ganesh Sittampalam)
 * switch to sandi from dataenc (Daniil Frumin)
 * remove hack to enable arbitrary protocols via env variables (Guillaume Hoffmann)
 * fixed the following bugs:

    * 1807: clarify help of PAGER, DARCS_PAGER (Guillaume Hoffmann)
    * 2258: improve patch index error message with suggestion (Guillaume Hoffmann)
    * 2269: push hijack test to suspend time (Eric Kow)
    * 2276: Keep track of patch hijack decisions (Eric Kow)
    * 2138: report conflicting files in whatsnew -s (Guillaume Hoffmann)
    * 2393: remove whatsnew functionality from annotate (Guillaume Hoffmann)
    * 2400: use async package to keep track of unpack threads (Ben Franksen)
    * 2459: fall back to writing the file if createLink fails (Ben Franksen)
    * 2479: root dir most not be among the sources of a move (Ben Franksen)
    * 2481: expose API for 'darcs diff' command (Ganesh Sittampalam)
    * 2486: obliterate --not-in-remote -q should be more quiet (Ben Franksen)
    * 2489: dequote filepaths while importing from git (Guillaume Hoffmann)
    * 2494: output of darcs record with file arguments (Ben Franksen)


Darcs 2.10.3, 29 January 2016

 * implement weak repository hash and show it in "darcs show repo"
 * implement "whatsnew --machine-readable" for more parsability
 * enhance Git import: empty directories, unescape file names, unnamed commits
 * make commit an alias for record
 * expose API for "darcs diff"
 * force grep to treat output of locale as text
 * bump dependencies: vector, process, HUnit, binary, transformers, time, HTTP,
   transformers-compat


Darcs 2.10.2, 9 November 2015

 * switch from dataenc (deprecated) to sandi
 * finish updating help strings with new command names
 * make patch selection lazier in presence of matchers
 * clean contrib scripts
 * switch patches retrieval order when using packs
 * disable mmap on Windows
 * enhance darcs send message
 * fix quickcheck suite
 * optimize patch apply code memory use
 * shorter README with quickstart instructions
 * fixed the following bugs:

    * 2457: fix darcs-test command line options
    * 2463: building darcs on powerpc
    * 2444: added default interactivity parameter to isInteractive


Darcs 2.10.1, 9 July 2015

 * generalized doFastZip for darcsden support
 * support terminfo 0.4, network 2.6, zlib 0.6, quickcheck 2.8 and
   attoparsec 0.13
 * errorDoc now prints a stack trace (if profiling was enabled)
 * beautified error messages for command line and default files
 * fixed the following bugs:

    * 2449: test harness/shelly: need to handle mis-encoded/binary data
    * 2423: diff only respecting --diff-command when a diff.exe is present
    * 2447: get contents of deleted file
    * 2307: add information about 'darcs help manpage' and
     'darcs help markdown'
    * 2461: darcs log --repo=remoterepo creates and populates
      _darcs
    * 2459: cloning remote repo fails to use packs if cache is
      on a different partition


Darcs 2.10.0, 19 April 2015

 * Important changes in Darcs 2.10

   * darcs rebase: enable deep amending of history
   * darcs pull --reorder: keep local-only patches on top of mainstream patches
   * darcs dist --zip: generate a zip archive from a repository
   * patch bundle contexts are minimized by default.
   * darcs convert export/import for conversion
     to/from VCSes supporting the fast-export protocol
   * darcs test --backoff: exponential backoff test strategy,
     faster than bisect on big repositories
   * work normally on sshfs-mounted repositories
   * automatic detection of file/directory moves, and of token replaces
   * patience diff algorithm by default
   * interactive mode for whatsnew
   * tag --ask-deps: create tags that may not include some patches
   * add a last question after all patches have been selected to confirm
     the whole selection
   * command names:
     * `clone` is the new name of `get` and `put`
     * `log`   is the new name of `changes`
     * `amend` is the new name of `amend-record`
   * show output of `log` into a pager by default
   * the output of `log` is more similar to git's:
     * show patch hash in UI (hash of the patch's metadata)
     * put author and date on separate lines
   * enable to match on patch hash prefix with -h and --hash
   * better messages:
     * better error messages for http and ssh errors
     * `init`, `add`, `remove`, `move` and `replace` print
       confirmation messages
   * `rollback` only happens in the working copy
   * `darcs send` no longer tries to send a mail by default
   * when no patch name given, directly invoke text editor
   * use nano as default text editor instead of vi
   * keep log files for patch name and mail content in _darcs
   * `optimize` and `convert` are now supercommands
   * improve `darcs help environment` and `darcs help markdown`
   * warn about duplicate tags when creating a new one
   * allow `darcs mv` into known, but deleted in working, file
   * improve` --not-in-remote`, allowing multiple repos and use default
   * faster `darcs diff`
   * faster `log` and `annotate` thanks to patch index data structure
   * faster push via ssh by using compression
   * cloning to an ssh destination (formerly `darcs put`) is more efficient
   * faster internal representation of patch hashes
   * when cloning from http, use packs in a more predictable way
   * store global cache in bucketed format
   * require and support GHC 7.4 to 7.10

 * Other issues resolved in Darcs 2.10

   * 346: implement "patience diff" from bzr
   * 642: Automatic detection of file renames
   * 822: generalized the IO Type for better error messages and exception handling
   * 851: interactive mode for whatsnew
   * 904: Fix record on Linux/FUSE/sshfs (fall back to sloppy locks automatically)
   * 1066: clone to ssh URL by locally cloning then copying by scp
   * 1268: enable to write darcs init x
   * 1416: put log files in tempdir instead of in working dir
   * 1514: send --minimize-context flag for send
   * 1624: bucketed cache
   * 1828: file listing and working --dry-run for mark-conflicts
   * 1987: Garbage collection for inventories and patches
   * 2181: put cache in $XDG_CACHE_HOME (~/.cache by default)
   * 2193: make that finalizeTentativeChanges no longer run tests
   * 2198: move repo testing code to Darcs.Repository.Test
   * 2200: darcs replace complains if no filepaths given
   * 2204: do not send mail by default
   * 2237: prevent patch index creation for non-hashed repos
   * 2235: Accept RFC2822 dates
   * 2246: add default boring entry for emacs session save files
   * 2253: attempting to use the patch index shouldn't create it on OF repos
   * 2278: Document default value for --keep-date / --no-keep-date
   * 2199: getMatchingTag needs to commute for dirty tags
   * 2247: move patch index creation into the job running code
   * 2238: let optsModifier remove all occurrences of LookForAdds
   * 2236: make 'n' an alias for 'q' in lastregret questions
   * 2155: Expurgate the non-functional annotate --xml-output option
   * 2248: always clean up rebase-in-progress state
   * 2270: fixed darcs changes -i --only-to-files
   * 2282: don't allow remote operations to copy the rebase patch
   * 2287: obliterate -O doesn't overwrite existing file.
   * 2227: move the rebase patch to the end before an amend-record
   * 2277: rebase suspend and unsuspend supports --summary.
   * 2311: posthook for 'get' should run in created repo
   * 2312: posthooks for 'record' and 'amend-record' should receive DARCS_PATCHES
   * 2163: new option for amend, select author for patch stealing.
   * 2321: when no patch name given, directly invoke text editor
   * 2320: save prompted author name in ~/.darcs/author instead of ./_darcs/prefs/author
   * 2250: tabbing in usageHelper - pad by max length of command name
   * 2309: annotate includes line numbers
   * 2334: fix win32 build removing file permission functions
   * 2343: darcs amend-record does not record my change
   * 2335: one liner when adding tracked files if not verbose
   * 2313: whatsnew -l: Stack space overflow
   * 2347: fix amend-record --prompt-long-comment
   * 2348: switch to cabal's test framework
   * 2209: Automatically detect replace
   * 2332: ignore case of characters in prompt
   * 2263: Option --set-scripts-executable is not properly documented
   * 2367: rename amend-record to amend, make --unrecord more visible
   * 2345: solution using cabal's checkForeignDeps
   * 2357: switching to regex-compat-tdfa for unicode support
   * 2379: only use packs to copy pristine when up-to-date
   * 2365: correctly copy pristine in no-working-dir clones
   * 2244: darcs tag should warn about duplicate tags
   * 2364: don't break list of 'bad sources'
   * 2361: optimize --reorder runs forever with one repository
   * 2364: fix file corruption on double fetch
   * 2394: make optimize a supercommand
   * 2396: make convert a supercommand and enhance help strings
   * 2314: output-auto-name in defaults file
   * 2388: check if inventories dir has been created
   * 2249: Rename isFile to isValidLocalPath and WorkRepoURL to WorkRepoPossibleURL
   * 2153: allow skipping backwards through depended-upon patches
   * 2380: allow darcs mv into known, but deleted in working, file
   * 2403: need to avoid moving the rebase patch to the end
   * 2409: implement darcs rebase apply
   * 2385: invoke pager without temporary file
   * 2333: better error message when pushing and darcs not in path


Darcs 2.8.5, 25 August 2014

 * GHC 7.8 support
 * Resolved issue2364: Download problems with GHC 7.8
 * Support matching on short patch hash
 * Resolved issue2345: Fix bug where configure script reported missing libiconv rather than libcurl


Darcs 2.8.4, 7 February 2013

 * GHC 7.6 support
 * Resolved issue2199: get --tag can include extra patches
 * Removed the --xml-output option to annotate which has been non-functional for a while


Darcs 2.8.3, 4 November 2012

 *  Tweak the library to avoid a C symbol clash with cryptohash


Darcs 2.8.2, 2 September 2012

 * Addition to the Darcs API to support darcsden


Darcs 2.8.1, 14 May 2012

 * Bumped mtl dependency
 * Updated contact details to use development mailing list


Darcs 2.8, 22 April 2012

 * Important changes in Darcs 2.8

   * Local support for the legacy "old-fashioned" repository format
        has been removed.
      * You can still work with remote repositories in this format
      * Local repositories can still be upgraded via "darcs optimize --upgrade".
   * "darcs annotate" is now significantly faster and uses less memory, although it still slows down linearly with repository size.
   * An experimental option is available to speed up HTTP downloads.
      * You can now use "darcs optimize --http" to create a repo optimized for HTTP downloads, and use "darcs get --packs" to download such a repo.
      * There are still some known issues with it, so the feature is not enabled by default.
   * Darcs now supports multiple email addresses for the author in the global prefs file. So, if you use different identities for home and work, you can now easily select between them. (issue1530)
   * The -o/-O options for obliterate which were removed in 2.5 are back.
   * "darcs status" has been added as a hidden alias for "darcs whatnew -ls" to ease the transition from some other source control systems. (issue182)
   * "darcs amend-record" now has the option --unrecord to allow individual changes to be unrecorded. (issue1470). This can also be used as "darcs amend-unrecord".
   * "darcs amend-record"'s interactive selection now supports 'k' for going back to a previous patch.
   * "darcs dist" now has the option --set-scripts-executable. (issue734)
   * pushing to ssh:// URLS is now supported. eg: darcs push ssh://code.haskell.org/foo
   * If a test fails during darcs record, you now have the option to record it anyway. (issue332)
   * Hunk-splitting now works in "darcs revert" (issue114)
   * Sending patches by email is now more robust and human friendly. We display a cleaner version in plain text for humans, and have added a complete version of the patch for "darcs apply". (issue1350)
   * "darcs send" now tries to report the right character set in the email headers, and has the option --charset to override this.
   * A new environment variable has been added to help control network connections: DARCS\_CONNECTION\_TIMEOUT. See the manual for details.
   * The --ephemeral and --partials options to "darcs get" has been removed. "darcs check --partial" has also been removed.
   * "darcs rollback" now has a --no-record option, to be used when you only want to update the working directory.
   * The --nolinks option for "darcs get" was removed.
   * The "--old" flag has been removed for "darcs init", "darcs get" and "darcs put".
   * "darcs resolve" has been removed an alias for "darcs mark-conflicts".
   * "darcs init" and "darcs get" now have a --no-working-dir option.
   * Conflicts are now marked with red in the terminal. (issue1681)
   * ssh ControlMaster support has been removed.
   * ssh stderr output is now passed through, making it easier to diagnose problems. (issue845)
   * Interactive selection now has a 'g' command to go to the first patch.
   * The --unified flag is now available for record, end-record, revert and unrevert (issue1166)
   * darcs now has a "darcs test" command for running whatever test-suite is associated with the repository.

 * Other issues resolved in Darcs 2.8

   * 1266: "darcs init" now warns when run inside another darcs repo.
   * 1344: When using darcs send, let users know sooner if they won't eventually be able to send
   * 1397: darcs changes /bad /paths no longer lists all changes
   * 1473: problem with annotate --repodir
   * 1551: Now we only use 'file content did not change' prompt for darcs send
   * 1558: xml output for patch hash no longer includes "gz" extension
   * 1599: automatically expire unused caches
   * 1637: When darcs get fails due to a HTTP failure, port the actual HTTP failure.
   * 1640: darcs apply --verbose docs have been improved
   * 1661: darcs add --quiet is quieter
   * 1705: fix error with unhandled --index in "darcs show contents"
   * 1714: provide more sensible behavior when combining an "ALL" default with a default for a specific sub-command
   * 1727: a better diagnostic message is provided when the user tries to move the root of the repository
   * 1740: "darcs mv" now gracefully the handles the case where the user first does an "mv" on a directory and then follows up with a "darcs mv"
   * 1804: The diagnostic message "getSymbolicLinkStatus: does not exist" has been improved
   * 1883: rename --patch-name option to --name. This is usually used in darcs record in shorthand (-m) form. This rename is aimed at eliminating the confusion with the patch matcher, which bites amend-record and rollback users.
   * 1884: darcs get was wrongly reporting "getting lazy repository" when you hit C-c
   * 1908: try to create a global cache before checking its availability
   * 1922: Fixed bug where obliterate -o was producing incorrect bundles in some cases
   * 1923: bad source warning mechanism no longer warns about sources outside your control
   * 1932: Handling of files with colons is improved
   * 1965: attempting "darcs move" on non-repo files now gives a sensible error
   * 1977: "darcs repair" now no longer complains if the pristine.hashed directory is missing
   * 1978: Improve handling of the _darcs/format file with "darcs get"
   * 1984: "darcs convert" now gives a better error message for invalid repo names
   * 2013: "darcs send" no longer ignores --to (or default recipient) with --context
   * 2021: character encoding handling has been improved
   * 2041: "darcs add" no longer follows directory symlinks
   * 2054: The behavior when combining --boring with a boring file has been improved
   * 2066: "darcs record" better handles adding deleted and added files passed on the command line
   * 2067: darcs diff no longer outputs blank lines when non-existent files are given
   * 2076: "darcs move myfile" into directory confuses darcs
   * 2077: "darcs mv myfile" into directory no longer confuses darcs
   * 2079: "darcs put" now does --set-default by default




Darcs 2.5.2, 14 March 2011

 * Important changes in Darcs 2.5.2

   * compatible with Haskell Platform 2011.2.0.0
   * bump parsec dependency for HP compatibility
   * fix regression allowing to add files inside boring directories

 * Issues resolved in Darcs 2.5.2

   * 2049: Darcs regression: 2.5 creates a broken patch

Darcs 2.5.1, 10 February 2011

 * Important changes in Darcs 2.5.1

   * original text is included in conflict marks
   * GHC 7.0 is supported
   * the version of GHC is restricted in the cabal file
   * warning message about old-fashioned repositories points to wiki
   * non-repository paths are guarded
   * library API: program name is configurable
   * darcs send prints the remote repository address right away
   * informational message about --set-default is disabled with --no-set
   * _darcs/format is correctly handled on get
   * linking libdarcs on Windows is fixed

 * Issues resolved in Darcs 2.5.1

   * 1978: get does not correctly treat _darcs/format
   * 2003: Message about --set-default should be optional
   * 2008: build with GHC 7.0
   * 2015: linking with libdarcs broken under Windows (2.5.0)
   * 2019: allow building with mtl 2
   * 2035: darcs accepts fake subpaths (relative paths outside of the repo)

Darcs 2.5, 30 October 2010:

 * Important changes in Darcs 2.5

   * trackdown can now do binary search with the --bisect option
   * darcs always stores patch metadata encoded with UTF-8
   * diff now supports the --index option
   * amend-record now supports the --ask-deps option
   * apply now supports the --match option
   * amend-record has a new --keep-date option
   * inventory-changing commands (like record and pull) now operate in
     constant time with respect to the number of patches in the repository
   * the push, pull, send and fetch commands no longer set the default
     repository by default
   * the --edit-description option is now on by default for the send command

 * Issues resolved in Darcs 2.5

   * 64:   store metadata as UTF-8
   * 121:  add --ask-deps support to amend-record
   * 643:  darcs send -o outputs remote repo email address
   * 1159: avoid bogus repository cache entries
   * 1176: caches interfere with --remote-repo flag
   * 1208: add trackdown --bisect
   * 1210: global cache gets recorded in _darcs/prefs/sources
   * 1232: darcs convert copies _darcs/prefs/prefs
   * 1250: check for newlines in setpref values
   * 1277: percolate repository format errors correctly
   * 1288: the main darcs code now compiles and runs with witnesses
   * 1290: support diff --index
   * 1337: don't show unrelated patches in darcs changes on untracked path
   * 1389: change predist pref to point people to use 'cabal sdist'
   * 1427: accept gzipped patch bundles in darcs apply
   * 1456: make dist write more portable archives
   * 1473: make annotate accept '.' as argument
   * 1503: prefer local caches to remote ones
   * 1713: shorter interactive prompts
   * 1716: allow mail header lines of all whitespace in test
   * 1719: do not back up files when no conflict markers are inserted
   * 1726: don't consider all files with _darcs prefix boring
   * 1739: make ColorPrinter handle characters > 255
   * 1763: use correct filename encoding in conflictors
   * 1765: refuse to remove non-tracked directories recursively
   * 1769: add support for --match 'comment ...'
   * 1784: push and pull print remote address right away
   * 1815: work around Cabal sdist permissions issue
   * 1817: fix support for external merge tools
   * 1824: avoid PACKAGE_VERSION under Windows
   * 1825: do not omit important prims in unrecordedChanges w/ files
   * 1860: (un)applying move patches doesn't corrupt pristine
   * 1861: fix typo in --no-boring help
   * 1874: recognise network tests on cabal test command line
   * 1875: avoid accidentally setting default
   * 1879: notice unexpected commute failure on merge
   * 1887: add a missing newline to --list-options output
   * 1893: move fields of conditional builds within scope of condition
   * 1898: notify user when they can use set-default
   * 1913: sort changes in treeDiff

Darcs 2.4.4, 9 May 2010

 * Important changes in Darcs 2.4.4

   * darcs builds on Windows with GHC 6.10 (and GHC 6.12).
   * darcs (built with GHC 6.12 on Windows) works with SSH again

 * Issues resolved in Darcs 2.4.4

   * 1814: Apply binary mode to ssh process and patch file handles.


Darcs 2.4.3, 9 May 2010

 * Important changes in Darcs 2.4.3

   * darcs builds on Windows with GHC 6.12.

Darcs 2.4.2, 8 May 2010

 * Important changes in Darcs 2.4.2

   * darcs will no longer give "mmap of _darcs_index" errors on Windows
   * darcs convert performance regression (against 2.3.x) solved
   * darcs get --partial no longer produces inconsistent repositories

 * Issues resolved in Darcs 2.4.2

   * 1761: mmap of '_darcs/index' failed on Windows mapped drives
   * 1814: include contrib/darcs-errors.hlint in release tarball
   * 1823: read (mmap) _darcs/index file correctly on Windows
   * 1826: error building on Windows with GHC 6.12
   * 1837: inconsistent repository upon darcs get --partial

Darcs 2.4.1, 31 March 2010

 * Important changes in Darcs 2.4.1

   * darcs works again on Windows shared directories
   * missing documentation and test files have been added to the tarball
   * darcs will no longer give errors about a nonexistent file when compiled
     with the wrong mmap
   * moving files between directories can no longer break the directory index
   * darcs handles the case that someone uses 'remove -r' on an
     untracked directory

 * Bugs Fixed in Darcs 2.4.1

   * 1750: uncover help text for 'darcs show pristine'
   * 1753: restrict mmap to version used by hashed-storage
   * 1754: fix issue458.sh on systems with xattrs
   * 1756: moving files between directories breaks index
   * 1757: add test files to distribution tarball
   * 1765: refuse to remove non-tracked directories recursively
   * 1767: send CRC erros to standard error


Darcs 2.4, 27 February 2010

 * Important changes in Darcs 2.4

   * Use fast index-based diffing everywhere (Petr)
   * Interactive patch splitting (Ganesh)
   * An 'optimize --upgrade' option to convert  to hashed format in-place
     (Eric)
   * Hunk matching (Kamil Dworakowski, tat.wright)
   * Progress reporting is no longer deceptive (Roman)
   * A 'remove --recursive' option to remove a directory tree from revision
     control (Roman)
   * 'show files' accepts arguments to show a subset of tracked files (Luca)
   * A '--remote-darcs' flag for pushing to a host where darcs isn't called
     darcs
   * Many miscellaneous Windows improvements (Salvatore, Petr and others)
   * 'darcs send' now mentions the repository name in the email body (Joachim)
   * Handle files with boring names in the repository correctly (Petr)
   * Fix parsing of .authorspellings file (Tomáš)
   * Various sane new command-line option names (Florent)
   * Remove the '--checkpoint' option (Petr)
   * Use external libraries for all UTF-8 handling (Eric, Reinier)
   * Use the Haskell zlib package exclusively for compression (Petr)

 * Bugs Fixed in Darcs 2.4

   *  183: do not sort changes --summary output
   *  223: add --remote-darcs flag to specify name of remote darcs executable
   *  291: provide (basic) interactive patch splitting
   *  540: darcs remove --recursive
   *  835: 'show files' with arguments
   * 1122: get --complete should not offer to create a lazy repository
   * 1216: list Match section in ToC
   * 1224: refuse to convert a repo that's already in darcs-2 format
   * 1300: logfile deleted on unsucessful record
   * 1308: push should warn about unpulled patches before patch-selection
   * 1336: sane error message on --last "" (empty string to numbers parser)
   * 1362: mention repo name in mail send body
   * 1377: getProgname for local darcs instances
   * 1392: use parsec to parse .authorspelling
   * 1424: darcs get wrongly reports "using lazy repository" if you ctrl-c 
           old-fashioned get
   * 1447: different online help for send/apply --cc
   * 1488: fix crash in whatsnew when invoked in non-tracked directory
   * 1548: show contents requires at least one argument
   * 1554: allow opt-out of -threaded (fix ARM builds)
   * 1563: official thank-you page
   * 1578: don't put newlines in the Haskeline prompts
   * 1583: on darcs get, suggest upgrading source repo to hashed
   * 1584: provide optimize --upgrade command
   * 1588: add --skip-conflicts option
   * 1594: define PREPROCHTML in makefile
   * 1620: make amend leave a log file when it should
   * 1636: hunk matching
   * 1643: optimize --upgrade should do optimize
   * 1652: suggest cabal update before cabal install
   * 1659: make restrictBoring take recorded state into account
   * 1677: create correct hashes for empty directories in index
   * 1681: preserve log on amend failure
   * 1709: fix short version of progress reporting
   * 1712: correctly report number of patches to pull
   * 1720: fix cabal haddock problem
   * 1731: fix performance regression in check and repair
   * 1741: fix --list-options when option has multiple names
   * 1749: refuse to remove non-empty directories


Darcs 2.3.1, 20 Sep 2009

 * Important changes in Darcs 2.3.1

   * Fix bugs introduced by optimizations in 2.3.0 (Petr, Salvatore)
   * Documentation improvements (Taylor, Trent)
   * Remove autoconf build system (Petr)

 * Bugs Fixed in Darcs 2.3.1

   See http://bugs.darcs.net/issueN for details on bug number N.

   * issue1373 darcs manual wrongly promises [^ ] is a valid token spec (Trent)
   * issue1478: document summary mnemonics (Trent)
   * issue1582 DeleteFile: permission denied (Access is denied.) (Salvatore)
   * issue1507 whatsnew does not use 'current' when 'pristine' is missing (Petr)


Darcs 2.3.0, 23 Jul 2009

 * Important changes in Darcs 2.3.0

   * Lots and lots of documentation changes (Trent).
   * Haskeline improvements (Judah).
   * Cabal as default buildsystem (many contributors).
   * Fixes in darcs check/repair memory usage (Bertram, David).
   * Performance improvement in subtree record (Reinier).
   * New option: --summary --xml (Florian Gilcher).
   * New option: changes --max-count (Eric and Petr).
   * Fix changes --only-to-files for renames (Dmitry).
   * Performance fix in "darcs changes" (Benedikt).
   * Hardlinks on NTFS (Salvatore).
   * Coalesce more changes when creating rollbacks (David).
   * New unit test runner (Reinier).
   * Inclusion of darcs-shell in contrib (László, Trent).
   * Author name/address canonisation: .authorspellings (Simon).
   * Working directory index and substantial "darcs wh" optimisation (Petr).
   * New command: "darcs show index" (Petr).
   * Gzip CRC check and repair feature (Ganesh).

 * Bugs Fixed in Darcs 2.3.0

   See http://bugs.darcs.net/issueN for details on bug number N.

   *  948 darcsman (Trent)
   * 1206 countable nouns (Trent)
   * 1285 cabal test v. cabal clean (Trent)
   * 1302 use resolved, not resolved-in-unstable (Trent)
   * 1235 obliterate --summary (Rob)
   * 1270 no MOTD for --xml-output (Lele)
   * 1311 cover more timezones (Dave)
   * 1292 re-encoding haskeline input (Judah)
   * 1313 clickable ToC and refs in PDF manual Trent)
   * 1310 create merged \darcsCommand{add} (Trent)
   * 1333 better "cannot push to current repository" warning (Petr)
   * 1347 (autoconf) check for unsafeMMapFile if mmap use enabled (Dave)
   * 1361 specify required includes for curl in cabal file (Reinier)
   * 1379 remove libwww support (Trent)
   * 1366 remove unreachable code for direct ncurses use (Trent)
   * 1271 do not install two copies of darcs.pdf (Trent)
   * 1358 encode non-ASCII characters in mail headers (Reinier)
   * 1393 swap "darcs mv" and "darcs move" (Trent)
   * 1405 improve discoverability of global author file (Trent)
   * 1402 don't "phone home" about bugs (Trent)
   * 1301 remove obsolete zsh completion scripts (Trent)
   * 1162 makeAbsolute is now a total function (Ben F)
   * 1269 setpref predist - exitcode ignored bug (Ben M)
   * 1415 --edit-long-comment, not --edit-description, in help (Trent)
   * 1413 remove duplicate documentation (Trent)
   * 1423 complain about empty add/remove (Trent)
   * 1437 Implement darcs changes --max-count (Eric)
   * 1430 lazy pattern matching in (-:-) from Changes command module (Dmitry)
   * 1434 refactor example test (Trent)
   * 1432 refer to %APPDATA%, not %USERPROFILE% (Trent)
   * 1186 give a chance to abort if user did not edit description file (Dmitry)
   * 1446 make amend-record -m foo replace only the patch name (Dmitry)
   * 1435 default to get --hashed from a darcs-1.0 source (Trent)
   * 1312 update and reduce build notes (Trent)
   * 1351 fix repository path handling on Windows (Salvatore)
   * 1173 support hard links on NTFS (Salvatore)
   * 1248 support compressed inventories for darcs-1 repos (Ganesh)
   * 1455 implement "darcs help environment" (Trent)


Darcs 2.2.0, 16 Jan 2009

 * Important changes in Darcs 2.2.0

   * Support for GHC 6.10.
   * Improved Windows support.
   * Cabal is now supported as a build method for darcs.
   * Low-level optimisations in filesystem code.
   * Overhaul of the make-based build system.
   * Extensive manual and online help improvements.
   * Improved API documentation (haddock) for existing darcs modules.
   * Improvements in the testing infrastructure.
   * Improved performance for "darcs repair".
   * Improved robustness for "darcs check".
   * Numerous major and minor bug fixes, refactorings and cleanups.
   * When recording interactively it is now possible to list all
     currently selected hunks (command 'l').

   * It is now possible to specify --in-reply-to when using darcs send,
     to generate correct references in the mail header.

   * Repositories can no longer be created with --no-pristine-tree.
     This only affects the legacy darcs-1 repository format.

   * Experimental Darcs library, providing increase flexibility and
     efficiency to third-party utilities (compared to the command-line
     interface).  Only built via Cabal.  NOT a stable API yet.

 * Bugs Fixed in Darcs 2.2.0

   See http://bugs.darcs.net/issueN for details on bug number N.

   *  525 amend-record => darcs patches show duplicate additions
   *  971 darcs check fails (case sensitivity on filenames)
   * 1006 darcs check and repair do not look for adds
   * 1043 pull => mergeAfterConflicting failed in geteff (2.0.2+)
   * 1101 darcs send --cc recipient not included in success message
   * 1117 Whatsnew should warn on non-recorded files
   * 1144 Add darcs send --in-reply-to or --header "In-Reply-To:...
   * 1165 get should print last gotten tag
   * 1196 Asking for changes in /. of directory that doesn't exist...
   * 1198 Reproducible "mergeConflictingNons failed in geteff with ix"
   * 1199 Backup files darcs added after external merge
   * 1223 sporadic init.sh test failure (2.1.1rc2+472)
   * 1238 wish: darcs help setpref should list all prefs
   * 1247 make TAGS is broken
   * 1249 2.1.2 (+ 342 patches) local drive detection on Windows error
   * 1272 amend-record not the same as unrecord + record
   * 1273 renameFile: does not exist (No such file or directory)
   * 1223 sporadic init.sh test failure (2.1.1rc2+472)


darcs (2.1.2)

 * Quality Assurance: Disable a new test that was not yet working
   under Windows

 -- Eric Kow <kowey@darcs.net>  Mon, 10 Nov 2008 10:40:00 GMT

darcs (2.1.1)

 -- Eric Kow <kowey@darcs.net>  Mon, 10 Nov 2008 08:18:00 GMT

darcs (2.1.1rc2)

  * Portability: Removed accidental QuickCheck 2.1 configure check.
    Note that it may be required in a future version of darcs.

 -- Eric Kow <kowey@darcs.net>  Mon, 10 Nov 2008 11:17:00 GMT

darcs (2.1.1rc1)

  * Portability: GHC 6.10.1 support (Petr Ročkai, Eric Kow)

  * Bug Fix: Fix file handle leak and check for exceptions on process
    running on Windows (issue784, Salvatore Insalaco)

  * Quality Assurance: Consolidated regression test suites using
    shell scripts only (Eric Kow, Tommy Petterson, Matthias Kilian)

 -- Eric Kow <kowey@darcs.net>  Mon, 10 Nov 2008 09:49:00 GMT

darcs (2.1.0)

  * Bug Fix: Eliminate a 'same URLs with different parameters' error when
    fetching files over HTTP (issue1131, Dmitry Kurochkin)

  * User Experience: Corrections to the default boring file (Ben Franksen)

  * Bug Fix: Fix the %a option in darcs send --sendmail-command (Ben Franksen)

  * Bug Fix: Do not obscure the SSH prompts or text editor output with
    progress reporting (issue1104, issue1109, Dmitry Kurochkin, David Roundy)

  * Bug Fix: pull --intersection work now works as advertised (Tommy
    Pettersson)

 -- Eric Kow <E.Y.Kow@brighton.ac.uk>  Sun, 09 Oct 2008 12:05:32 GMT

darcs (2.1.0pre3)

  * Bug Fix: Eliminate an error merging repeated conflicts in darcs-2
    repositories (issue1043, David Roundy)

  * New Feature: Hide 'Ignore-this:' lines which will be generated by future
    versions of darcs to prevent patch-id collisions. (issue1102, Eric Kow,
    David Roundy)

  * Bug Fix: Support darcs repositories which have symbolic links in their
    paths (issue1078, Dmitry Kurochkin)

  * Bug Fix: Make ssh connection sharing (darcs transfer-mode) work with
    old-fashioned repositories (issue1003, David Roundy)

 -- Eric Kow <E.Y.Kow@brighton.ac.uk>  Sun, 02 Oct 2008 09:12:41 GMT

darcs (2.1.0pre2)

  * IMPORTANT: Create darcs-2 repositories by default in darcs init (issue806,
    David Roundy)

  * User Experience: Do not allow users to add files to a darcs repository if
    their filenames would be considered invalid under Windows. This can be
    overridden with the --reserved-ok flag (issue53, Eric Kow)

  * Bug Fix: Do not leave behind a half-gotten directory if darcs get fails
    (issue1041, Vlad Dogaru, David Roundy)

  * User Experience: notice when you are trying to pull from a seemingly
    unrelated repository, that is one with a sufficiently different history.
    This can be overridden with the --allow-unrelated-repos flag (Dmitry
    Kurochkin, David Roundy)

  * Bug Fix: Fix hang after a user input error (for example, EOF) (Judah
    Jacobson)

  * Quality Assurance: Improvements to documentation and online help (Simon
    Michael)

 -- Eric Kow <E.Y.Kow@brighton.ac.uk>  Sun, 25 Sep 2008 08:10:49 GMT

darcs (2.0.3pre1)

  * New Feature: Optional readline-like functionality when compiled with the
    haskeline package (Judah Jacobson, Gaëtan Lehmann)

  * Bug Fix: No more spurious pending patches (issue709, issue1012, David
    Roundy)

  * Bug Fix: darcs get --to-match now works with hashed repositories
    (issue885, David Roundy)

  * User Experience: Catch mistakes in _darcs/prefs/defaults (issue691, Dmitry
    Kurochkin)

  * User Experience: Improved support for darcs send over http (see also
    tools/upload.cgi) (Dmitry Kurochkin, David Roundy)

  * Bug Fix: Recognize user@example.com: as an ssh path, that is, not
    requiring a path after the server component. (David Roundy)

  * New Feature: Accept an optional directory argument in darcs send
    --output-auto-name (Dmitry Kurochkin)

  * User Experience: New --no-cache option to help debug network issues
    (issue1054, Dmitry Kurochkin)

  * Performance: New --http-pipelining and --no-http-pipelining flags. Passing
    --http-pipelining to darcs can make darcs get and pull faster over HTTP.
    Due to a libcurl bug, this is not the default option unless darcs is
    compiled with libcurl 7.19.1, due 2008-11. (Dmitry Kurochkin)

  * Bug Fix: Eliminate hanging and crashes while fetching files over HTTP
    (issue920, issue977, issue996, issue1037, Dmitry Kurochkin)

  * Security: Fix some insecure uses of printfs in darcs.cgi (Steve Cotton)

  * Bug Fix: Handle filepaths in a simpler and more robust fashion. This fixes
    relative filepaths and recognition of symbolic links and avoids possible
    future bugs (issue950, issue1057, David Roundy, Dmitry Kurochkin)

  * Bug Fix: Make darcs diff --patch work even if the patch is within a tag
    (issue966, darcs 2 regression, Dmitry Kurochkin)

  * Quality Assurance: Extend use of Haskell's GADTs to most of the darcs
    code, fixing many potential bugs along the way (Jason Dagit, David Roundy)

  * Quality Assurance: Several improvements to darcs code quality (Petr
    Ročkai)

  * Bug Fix: Correct assumptions made by darcs about Windows file size types
    (issue1015, Simon Marlow, Ganesh Sittampalam)

  * Bug Fix: Support case insensitive file systems using hashed repositories
    in darcs repair (partial issue971, Petr Ročkai). IMPORTANT: This
    introduces a memory use regression, which affects large repositories. We
    found that doing a darcs repair on the GHC repository requires a machine
    with 2 GB of RAM. The regression is well-understood and should be solved
    in the next darcs release. In the meantime we felt that the improved
    robustness was worth the performance trade-off.

  * Quality Assurance: Simplify building darcs on Windows by optionally using
    the zlib and terminfo Haskell packages (Ganesh Sittampalam, Petr Ročkai)

  * User Experience: Better error reporting when patches that should commute
    fail to do so. (Jason Dagit)

  * New Feature: --match "touch filenames", for example --match 'touch
    foo|bar|splotz.*(c|h)' (issue115, Dmitry Kurochkin)

  * User Experience: Improve debugging and error messages in HTTP code (Dmitry
    Kurochkin, David Roundy)

  * Bug Fix: Ensure that darcs responds to Ctrl-C on Window, even if compiled
    with GHC < 6.10 (issue1016, Simon Marlow)

  * New Feature: darcs changes --context now also works with --human-readable
    and --xml-output (issue995, Dmitry Kurochkin)

  * Bug Fix: Always darcs send with context, as if --unified flag were used
    (was implemented in 2.0.2, but not consistently) (David Roundy)

  * Bug Fix: Make sure that darcs get --tag works even when the user hits
    Ctrl-C to get a lazy repository (Dmitry Kurochkin)

  * Quality Assurance: Improvements to documentation and online help, most
    crucially, user-focused help on upgrading to darcs 2. (Trent Buck, Lele
    Gaifax, Simon Michael, Max Battcher)

  * New Feature: darcs changes --number associates each patch with number,
    counting backwards (see the --index feature) (David Roundy)

  * New Feature: ability to match patches on index, for example, darcs changes
    --index=3-6 shows the last three to six patches (David Roundy)

  * User Experience: slightly reduce the verbosity of darcs pull --verbose
    (David Roundy)

 -- Eric Kow <E.Y.Kow@brighton.ac.uk>  Sun, 18 Sep 2008 02:36:45 GMT

darcs (2.0.2)

 -- David Roundy <droundy@darcs.net>  Sun, 24 Jun 2008 01:20:41 GMT

darcs (2.0.1)

  * Bug Fix: Make Ctrl-C work even though darcs is now compiled to use the
    threaded runtime (issue916, David Roundy)

  * New Feature: Include patch count in darcs --version, for example, 2.0.1 (+
    32 patches) (David Roundy)

  * Bug Fix: Avoid an error caused by renaming a file on case-insensitive
    file-systems (Eric Kow)

  * Bug Fix and New Feature: Improved XML output (Benjamin Franksen, Lele
    Gaifax, David Roundy)

  * User Experience: Always darcs send with context, as if --unified flag were
    used (David Roundy)

 -- David Roundy <droundy@darcs.net>  Sun, 23 Jun 2008 21:47:07 GMT

darcs (2.0.1rc2)

  * Performance: Faster strings, using Data.Bytestring by default (Gwern
    Branwen, Eric Kow, Ian Lynagh, David Roundy)

  * User Experience: On Windows, use MS-DOS 'edit' as a default editor if
    Emacs and friends are not available (Eric Kow)

  * Bug Fix: On Windows, notice when external programs fail to launch because
    they do not exist (Eric Kow)

  * New Feature: darcs put --no-set-default and --set-default (Nicolas
    Pouillard)

 -- David Roundy <droundy@darcs.net>  Sun, 13 Jun 2008 01:17:45 GMT

darcs (2.0.1rc1)

  * Bug Fix: Fix tag --checkpoint so that darcs get --partial can avoid
    fetching all patches (issue873, David Roundy)

  * User Experience: Better progress reporting [NB: darcs is now compiled with
    threaded runtime by default] (issue739, David Roundy, Bertram Felgenhauer)

  * Performance: Reduce memory usage of darcs put (David Roundy)

  * Bug Fix: Improved date matching (issue793, issue187, Eric Kow)

  * Performance: Fix an optimization in diff-detection (affects darcs whatsnew
    and record) (Pekka Pessi)

  * Quality Assurance: --enable-hpc for checking program coverage (Christopher
    Lane Hinson)

  * Bug Fix: Do not rollback if no primitive patches were selected (issue870,
    Eric Kow)

  * Bug Fix: Make it possible to --dry-run on repositories we cannot write to
    (issue855, Eric Kow, David Roundy)

  * Bug Fix: Avoid a race condition caused by cleaning out the pristine cache
    (issue687, David Roundy)

  * User Experience: When pushing, prints a small reminder when the remote
    repository has patches to pull (Eric Kow, David Roundy)

  * UI changes: --extended-help is now called --overview, no more
    --verify-hash, no more send --unified (David Roundy, Eric Kow)

  * User Experience: Show ssh's stderr output in case it wants to ask the user
    something (issue845, Eric Kow)

  * Bug Fix: Improved interaction with pager (David Roundy, Pekka Pessi, Eric
    Kow)

  * Bug Fix: darcs send -o - (Pekka Pessi)

  * Bug Fix: (regression) Re-enable darcs mv as a means of informing darcs
    about manual renames (issue803, David Roundy)

  * Bug Fix: Fix bugs related to use of threaded runtime (issue776, David
    Roundy)

  * Portability: Respect OS conventions in creation of temporary files (Eric
    Kow)

  * New Feature: Check for and repair patches which remove non-empty files
    (issue815, David Roundy)

  * Bug Fix: Make get --to-match work with hashed repositories (David Roundy)

  * Bug Fix: Conflict-handling with darcs-2 semantics (issue817, David Roundy)

  * Bug Fix: Make --ask-deps ask the right questions (Tommy Pettersson)

  * User Experience: Improved error messages and warnings (issue245, issue371,
    Nicolas Pouillard, David Roundy, Eric Kow)

  * New Feature: darcs trackdown --set-scripts-executable (Reinier Lamers)

  * Quality Assurance: Various improvements to documentation (issue76,
    issue809, Gwern Branwen, Lele Gaifax, Eric Kow, Nicolas Pouillard, David
    Roundy)

  * Bug Fix: Correct detection of incompatibility with future darcs (issue794,
    Eric Kow)

  * User Experience: Make darcs changes --interactive behave more like other
    interactive commands (Eric Kow)

  * Performance: Optimized handling of very large files (Gwern Branwen)

  * New Feature: Colorize added and removed lines, if the environment variable
    DARCS_DO_COLOR_LINES=True (Nicolas Pouillard)

  * New Feature: --remote-repodir flag to allow separate default repositories
    for push, pull and send (issue792, Eric Kow)

  * Performance: Optimized get --to-match handling for darcs 1 repositories
    (Reinier Lamers)

  * Bug Fix: Make changes --repo work when not in a repository (David Roundy)

  * New Feature: darcs changes --count (David Roundy)

 -- David Roundy <droundy@darcs.net>  Sun, 03 Jun 2008 12:43:31 GMT

darcs (2.0.0)

  * Fix silly bug which leads to darcs --version not showing release when it's
    a released version. (David Roundy)

 -- David Roundy <droundy@darcs.net>  Sun, 07 Apr 2008 15:06:38 GMT

darcs (2.0.0rc1)

 -- David Roundy <droundy@darcs.net>  Sun, 01 Apr 2008 15:44:11 GMT

darcs (2.0.0pre4)

  * When darcs encounters a bug, check version versus central server in order
    to decide whether to recommend that the user report the bug.

  * Display duplicate identical changes when using darcs-2 repository format.
    (Issue579)

  * Fix a bug in convert that lead to invalid tags in the converted
    repository. (Issue585)

  * Add an annoying warning when users run convert.

  * Numerous fixes to the time/date matching code, which should now work even
    in central Europe. (Eric Kow)

  * Add support for reading hashed repositories that use SHA256 hashes. The
    plan is to enable writing of SHA256 hashes in the next release. (David
    Roundy)

  * New Feature: Add a 'show authors' command (Eric Kow)

  * darcs.cgi improvements: Patch pages show "Who" and "When" some file
    annotation pages show "who" and "when" with a mouse-over. Also, darcs.cgi
    can now be hosted in a path containing The tilde character. (Zooko, Mark
    Stosberg)

  * User Experience: Improved and added many debugging, error and progress
    messages (David Roundy, Mark Stosberg, Eric Kow)

  * New Feature: New DARCS_PATCHES, DARCS_FILES and DARCS_PATCHES_XML
    environment variables are made available for the posthook system, allowing
    for more easier options to to integrate darcs with other systems. (David
    Roundy, Mark Stosberg)

  * Quality Assurance: Added and updated automated regression tests (Mark
    Stosberg, David Roundy, Eric Kow, Trent Buck, Nicolas Pouillard, Dave
    Love, Tommy Pettersson)

  * Bug Fix: Gzipped files stored in the repo are now handled properly (Zooko,
    David Roundy)

  * Quality Assurance: Various Documentation Improvements (issue347, issue55
    Mark Stosberg, Nicolas Pouillard, Marnix Klooster)

  * Bug Fix: With --repodir, commands could not be disabled (Trent Buck, David
    Roundy)

  * New Feature: tools/update_roundup.pl scripts allows the darcs bug tracker
    to be notified with a darcs patch resolving a particular issue is applied.
    A link to the patch in the web-based repo browser is provided in the
    e-mail notifying bug subscribers. (Mark Stosberg)

  * Internal: Begin work on memory efficiency improvements (David Roundy)

  * Performance: darcs is faster when identifying remote repos handling
    pending changes and running unrecord. (David Roundy)

  * Internal: Source code clean-up and improvements (David Roundy, Jason
    Dagit, Eric Kow, Mark Stosberg)

  * User Experience: A pager is used automatically more often, especially when
    viewing help. (Eric Kow)

  * Bug Fix: push => incorrect return code when couldn't get lock. (issue257,
    VMiklos, David Roundy, Eric Kow, Mark Stosberg)

  * Bug Fix: 'whatsnew' and 'replace' now work together correctly. (Nicolas
    Pouillard, David Roundy)

 -- David Roundy <droundy@darcs.net>  Sun, 21 Mar 2008 15:31:37 GMT

darcs (2.0.0pre3)

  * Fix issue 244, allowing users of darcs changes to specify the new name of
    a file that has an unrecorded mv. (David Roundy, Mark Stosberg, Tuomo
    Valkonen)

  * Fix issue 600, bug in darcs optimize --relink. (David Roundy, Trent Buck,
    Mark Stosberg, Tommy Pettersson)

  * Add a new framework for outputting progress messages. If darcs takes more
    than about one second to run a command, some sort of feedback should now
    be provided. (David Roundy)

  * Rewrite rollback, changing its behavior to be more useful. Rollback now
    prompts for a name for the new "rollback" patches. It also allows you to
    roll back multiple patches simultaneously, and to roll back only portions
    of the patches selected. Altogether, rollback is now more interactive, and
    should also be more useful. (David Roundy)

  * Bug Fix: date parsing is now improved (Mark Stosberg, David Roundy)

  * Performance: Improved speed of darcs pull on very large repos. (David
    Roundy)

  * Fix issue 586, but in darcs repair on hashed and darcs-2 repositories.
    (Nicolas Pouillard)

  * Improve docs for 'darcs init' (Mark Stosberg)

  * Fix typo in test partial.sh which made part of the tests for --partial
    invalid. (Mark Stosberg)

  * Document that darcs handles some types of binary files automatically.
    (issue55, Mark Stosberg)

  * Fix typo in a test that made it compare a file to itself. (Mark Stosberg)

  * Document that single quotes should be used in .darcs/defaults. (issue347,
    Mark Stosberg)

  * New Feature: Automatically create the the global cache if we define we
    want to use it. (David Roundy, Trent Buck)

  * Performance: Improved HTTP pipelining support (Dmitry Kurochkin)

  * Fix issue 571, build failure when termio.h is not found. (Dave Love)

 -- David Roundy <droundy@darcs.net>  Sun, 22 Jan 2008 20:06:12 GMT

darcs (2.0.0pre2)

  * Add instructions in documentation for how to view patches in Mutt (a mail
    reader). (Gwern Branwen)

  * Fix build on Solaris. (Dave Love)

  * Added "auto-optimize" support for hashed inventories, in that darcs
    automatically optimizes inventories when it seems wise (which is currently
    defined as "every time we modify the inventory").

  * Fix expensive performance bugs involved in conflict handling. Thanks to
    Peter for pointing these out!.

  * Fix reading of hashed repositories to avoid reading patches that we don't
    actually need (i.e. foolish inefficiency bug). Thanks to Simon for
    reporting these performance bugs.

  * Added a new --debug flag for debug output.

  * Added compatibility with ghc 6.4. At this point darcs 2 should work with
    any ghc from 6.4 to 6.8.2.

  * Fix bug where parsing of setpref patch called tailPS unnecessarily. (David
    Roundy)

  * Refactor parsing of commands and command line arguments. Implement hidden
    commands. (Eric Kow)

  * Use a single command to initialize a remote repository. This replaces the
    method of stringing together multiple commands with the shell-dependent &&
    operator. (Tristan Seligmann)

  * Allow for files in _darcs/inventories to be gzipped. This is not
    specifically related to issue553, but it fixes a regression introduced by
    the issue553 fix. (Issue553, Eric Kow)

  * Check for potential hash collision in writeHashFile. (Eric Kow)

  * Don't try to write hash file if it already exists, as you can not
    overwrite an open file on Windows. (Issue553, Eric Kow)

  * Close file handles when done reading it lazily. (Eric Kow)

  * Modernize and enhance buggy renameFile workaround by using the
    hierarchical library structure and only catching 'does not exist' errors.
    (Eric Kow)

  * Add "hidden" printer for decorating patches with color for easier reading
    when printed to screen during verbose or debug output, but hides (removes)
    the decoration when printing to the repository files. This is the
    counterpart of the invisible printer, which makes non-human-friendly patch
    contents invisible when printed to the screen. (David Roundy)

  * Add "hidden" printer, for printing things to screen but not file. (David
    Roundy)

  * Make darcs distinguish between repository sub paths and "normal" relative
    paths. Better handling of absolute paths. (Eric Kow)

  * Fix some bugs introduced by Better handling of file paths. (Eric Kow)

  * Handle corner case when polling from self. (issue354, issue358, Eric Kow)

  * Handle corner cases when pulling from current repository. (Issue354,
    Issue358, Eric Kow)

  * Fix bug in make_dotdots when pushing from a sub directory. (issue268, Eric
    Kow)

  * Fix bug in make_dotdots when pushing from a subdirectory. (Issue268, Eric
    Kow)

  * Better handling of file paths. Distinguish between paths to files
    belonging to the repository as well as not belonging to the repository,
    both in absolute and relative form. (Eric Kow)

  * Add path fixing to darcs send, and don't try sending to self. (issue427,
    Eric Kow)

  * Fix path issue for darcs send. (Issue427, Eric Kow)

  * Disable mmap under Windows. (issue320, Eric Kow)

  * Backup unmanaged working dir files as needed. (issue319, issue440, Eric
    Kow)

  * Backup unmanaged files in the working directory when they are overwritten
    by managed files with the same names in pulled or applied patches.
    (Issue319, Issue440, Eric Kow)

  * Offer some advice if sendmail failed. (issue407, Eric Kow)

  * Document behavior of "boring" managed files. (Issue259, Eric Kow)

  * Make Doc a newtype, so we can define a Show instance. (David Roundy)

  * Make make_changelog GHC 6.8 compliant. (Ganesh Sittampalam)

  * GHC 6.8 needs containers package. (Ganesh Sittampalam)

  * Configure hack to deal with openFd -> fdToHandle' renaming in GHC 6.8.
    (Ganesh Sittampalam)

  * Make makefile summarize calls to GHC when compiling. VERBOSE=1 turns the
    long format back on. (Eric Kow)

  * When building, print summarized call to GHC in makefile, instead of very
    long command lines with many boring options. VERBOSE=1 reverts to showing
    options again. (Eric Kow)

  * Add svg logo. (David Roundy)

  * Add mercurial files to the default boring file. (David Roundy)

  * Add patterns for mercurial files to default boring patterns. (David Roundy)

  * Define color versions of traceDoc and errorDoc for debugging. (David
    Roundy)

  * Clarify error message for --last. (issue537, Eric Kow)

  * Clarify in error message that darcs option --last requires a *positive*
    integer argument. (Issue537, Eric Kow)

  * Optimize getCurrentDirectorySansDarcs a little. (Eric Kow)

  * Never create temporary directories in the _darcs directory. (issue348,
    Eric Kow)

  * Never create temporary directories in the _darcs directory. (Issue348,
    Eric Kow)

  * Make revert short help less cryptic. (Eric Kow)

  * Make revert short help less cryptic. (Eric Kow)

  * Make --checkpoint short help more explicit. (issue520, Eric Kow)

  * Make --checkpoint short help more explicit. (Issue520, Eric Kow)

  * Add format infrastructure for darcs-2 repo format. (David Roundy)

  * Always optimize the inventory in 'darcs tag'. (Eric Kow)

  * Fix bug in Tag --checkpoint where the inventory was not updated. (Eric Kow)

  * Fix accidental regression of --no-ssh-cm flag. (Eric Kow)

  * Move conditional #include from Darcs.External to makefile. The GHC manual
    says that this is *not* the preferred option, but for some reason, the
    include pragmas seem to get ignored. Perhaps it is because the requirement
    that the pragmas be on the top of the file conflict with the #ifdef
    statements. In any case, this patch gets rid of the warning on MacOS X:
    warning: implicit declaration of function 'tgetnum'. (Eric Kow)

  * Pass CFLAGS to the assembler. E.g. -mcpu is essential on sparc. (Lennart
    Kolmodin)

  * Optimize 'darcs optimize --reorder'. (David Roundy)

  * Add a table of environmental variables to the manual. (issue69, Eric Kow)

  * Use System.Directory.copyFile for file copying. (Kevin Quick)

  * Implement darcs show contents command. It shows the contents of a file at
    a given version. (issue141, Eric Kow)

  * Make Changes --context --repodir work. (Issue467, Erik Kow)

  * Rename 'query' to 'show', but keep 'query' as an alias. (There is also an
    extra alias 'list' that means the same as show.) The subcommand 'query
    manifest' is renamed to 'show files', and does not list directories by
    default, unless the alias 'manifest' is used. (Eric Kow)

  * Support record -m --prompt-long-comment. (issue389, Eric Kow)

  * Hide the command 'unpull' in favor of 'obliterate'. (Eric Kow)

  * Make option --no-deps work again. It now also works for obliterate,
    unrecord, push and send. (issue353, Tommy Pettersson)

  * Make Record --ask-deps abort if user types 'q' instead of recording
    without explicit dependencies. User is now required to type 'd' (done). If
    the resulting patch is completely empty (no changes and no dependencies)
    the record is automatically canceled. (issue308, issue329, Kevin Quick)

  * Use pure record-access for PatchInfo in Patch.Info. (David Roundy)

  * Improve error message when unable to access a repository. (David Roundy)

  * Switch to using new Haskell standard library function cloneFile for
    copying files. (Kevin Quick)

  * Remove more GUI code. (Eric Kow)

  * Fix some --dry-run messages: "Would push" instead of "Pushing". (issue386,
    Eric Kow)

  * Ensure that logfile for record has trailing newline. (issue313, Eric Kow)

  * Add a stub command 'commit' that explains how to commit changes with
    darcs. (Eric Kow)

  * Makes non-repository paths in DarcsFlags absolute. (issue427, Zachary P.
    Landau)

  * Fix problem with missing newline in inventory, to simplify for third party
    scripts. (Issue412, Eric Kow)

  * Add all pulled repositories to _darcs/prefs/repos. (Issue368, Eric Kow)

  * Implement Apply --dry-run. (Issue37, Eric Kow)

  * Never change defaultrepo if --dry-run is used (issue186, Eric Kow)

  * Filter out any empty filenames from the command line arguments. (Issue396,
    Eric Kow)

  * Use prettyException in clarify_errors so we don't blame user for darcs'
    own errors. (Issue73, Eric Kow)

  * Rename command 'resolve' to 'mark-conflicts'. 'Resolve' remains as a
    hidden alias. (issue113, Eric Kow)

  * Make 'query manifest' list directories by default. (issue456, Eric Kow)

  * Allow --list-options even if command can not be run. (issue297, Eric Kow)

  * Make 'unadd' an alias for 'remove'. Make 'move' an alias for 'mv'. Add a
    stub for 'rm' that explains how to remove a file from a darcs repository.
    (issue127, Eric Kow)

  * Fix <supercommand> --help. (Issue282, Eric Kow)

  * New --nolinks option to request actual copies instead of hard-links for
    files. (Kevin Quick)

  * Harmonize capitalization in flags help. (Eric Kow)

  * Define datarootdir early enough in autoconf.mk.in. (Issue493, Eric Kow)

  * Fix a bug where Get --partial would use a checkpoint without detecting it
    was invalid. Checkpoints can for example become invalid after an Optimize
    --reorder. (issue490, David Roundy)

  * User Agent size limit for curl gets is removed. (Issue420, Kevin Quick)

  * Don't garb string parameters passed to libcurl, as required by the api
    specification. (Daniel Gorin)

  * Fix handling of --repo with relative paths. (Eric Kow)

  * Check for gzopen in zlib. curl depends on zlib and is detected prior to
    zlib by the configure file, but without the -lz flag on some versions.
    (Andres Loeh)

  * Switch to haskell's System.Process under Unix for execution of external
    commands; requires GHC 6.4. (Eric Kow)

  * Remove (some more) conflictor code. (Eric Kow)

  * Remove (unused) conflictor code. (David Roundy)

  * Support makefile docdir/datarootdir variables. (Dave Love)

  * Added prehooks that works the same as posthooks, but they run before the
    command is executed. This can for example be used to convert line endings
    or check character encodings before every Record. The darcs command aborts
    if the prehook exits with an error status. (Jason Dagit)

  * Use system instead of rawSystem for calling interactive cmds in Windows,
    which lets us support switches, for example, in DARCS_EDITOR. (Eric Kow)

  * add support for partial and lazy downloading of hashed repositories.
    (David Roundy)

  * Fix refactoring bug in Checkpoints where we sometimes looked for things in
    the wrong place. (David Roundy)

  * Fail on error in get_patches_beyond_tag. This will expose any bugs where
    we use this function wrongly. (As was the case in darcs check --partial
    with hashed inventories.) (David Roundy)

  * Restructure the source tree hierarchy from a mostly flat layout to one
    with directories and subdirectories that reflects the modularity of the
    source. (Eric Kow)

  * In tests, don't assume grep has -q and -x flags. (Dave Love)

  * Add --output-auto-name option to Send (Zachary P. Landau)

  * Added regression testing for the "pull --complement" operation. Updated
    documentation to explain why "darcs pull --complement R1 R1" is the same
    as "darcs pull R1" instead of the empty set. (Kevin Quick)

  * Change all "current" to "pristine" in manual and help texts. (Tommy
    Pettersson)

  * Added the ability to specify the --complement argument on the pull command
    as an alternative to --intersect and --union. When --complement is
    specified, candidate patches for a pull are all of the pullable patches
    that exist in the first repository specified but which don't exist in any
    of the remaining repositories (the set-theory complement of the first
    repository to the union of the remaining repositories). (Kevin Quick)

  * Fix bug where darcs would try to write temporary files in the root
    directory (/) if it couldn't find a repository root to write them in. Now
    it uses the current directory in that case. (issue385, Zachary P. Landau)

  * Make write_repo_format use the same syntax as read_repo_format when
    dealing with different repository formats. (Benedikt Schmidt)

  * Remove some unused functions from Population. (Eric Kow)

  * Use IO.bracket instead of Control.Exception.bracket in Exec, to restore
    the old way darcs works on *nix. (Eric Kow)

  * Import bracketOnError from Workaround instead of Control.Exception to
    support GHC 6.4. (Eric Kow)

  * Switch to haskell's System.Process under Windows for execution of external
    commands; requires GHC 6.4. (Simon Marlow)

  * Fix bug where darcs ignored command arguments in the VISUAL environment
    variable. (issue370, Benedikt Schmidt)

  * Make annotate work on files with spaces in the name. (Edwin Thomson)

  * Prettify exceptions in identifyRepository. (Juliusz Chroboczek)

  * QP-encode patch bundles transfered with the Put command. (Juliusz
    Chroboczek)

  * Fix bug in darcs get --tag that left cruft in pending. (David Roundy)

  * Fix bug when trying to 'darcs mv foo foo'. (issue360, David Roundy)

  * Separate comment from OPTIONS pragma for GHC 6.4 compatibility. (Eric Kow)

  * Make hashed inventories support optimize and reordering. (David Roundy)

  * Change all Maybe Patch to the new type Hopefully Patch, which is similar
    to Either String, for storing patches that may or may not exist. This
    should make it much easier to improve error reporting. (David Roundy)

  * Fix pending bug that broke several_commands.sh. (David Roundy)

  * Fix hashed inventory bug in Add. (David Roundy)

  * Make Get and Put reuse code for Initialize. This makes Put accept any
    flags that Init accepts. (David Roundy)

  * Fix new get to not mess up pending. (David Roundy)

  * External resolution can resolve conflicting adds. (Edwin Thomson)

  * Only copy the files that are needed for the resolution, when invoking an
    external resolution tool. This saves much time and resources on
    repositories with many files in them. (Edwin Thomson)

  * Change message in 'darcs check' from "applying" to "checking". (issue147,
    Tommy Pettersson)

  * Add code fore hashed inventories. (David Roundy)

  * New option for Diff: --store-in-memory. darcs diff usually builds the
    version to diff in a temporary file tree, but with --store-in-memory it
    will represent the files in memory, which is much faster (unless the tmp
    directory already is a ram disk). (Edwin Thomson)

  * Fix bug where duplicated file names on the command line would fool darcs.
    (issue273, Tommy Pettersson)

  * When recording with option --pipe, assume users local timezone if none is
    given, instead of UTC. Except if the date is given in raw patch format
    'yyyymmddhhmmss' it is still parsed as UTC. (issue220, Eric Kow)

  * Account for timezone information, e.g. in dates when recording with option
    --pipe. (issue173, Eric Kow)

  * Fix bug in refactoring of get. (David Roundy)

  * Refactor repository handling to allow truly atomic updates. (David Roundy)

 -- David Roundy <droundy@darcs.net>  Sun, 16 Dec 2007 20:16:47 GMT

darcs (1.0.9)

  * Make shell harness failures fatal in Makefile. (Eric Kow)

  * Bugfix, fix bug where we add a file but not its boring parent directory.
    (David Roundy)

  * Allow escaped quotes in 'quoted' for match text. (Dave Love)

  * Don't exit with failure when tests_to_run is used and there are no perl
    tests. (David Roundy)

  * Apply patches "tolerantly" to the working directory; don't quit, but print
    a warning for every problem and continue. This is a workaround for a bug
    in darcs where it sometimes fails to update the working directory. When
    darcs updates the working directory it has already successfully updated
    the inventory and the pristine cache, so the repository itself is not
    corrupted. However, an incomplete update to the working directory results
    in unintended differences between the working and pristine tree, looking
    like spurious unrecorded changes. These can be easily removed with 'darcs
    revert', but spurious changes have to be manually sorted out from real
    unrecorded changes. By darcs no longer quiting at the first problem, more
    of the working tree gets updated, giving less spurious changes and less
    manual work to fix the mess should the bug bite you. (issue434, Eric Kow,
    David Roundy)

  * Add a README file, created from HACKING. (issue287, Eric Kow)

  * New command, query tags (similar to 'darcs changes -t .) (Florian Weimer)

  * Include the query commands in the manual. (Florian Weimer)

  * The ssh control master is now off by default (it seems to hang on some
    large repositories). The option --disable-ssh-cm is replaced by the two
    options --ssh-cm and --no-ssh-cm (default). (Eric Kow)

  * Do not append a colon to host name when calling sftp. This does not solve
    all of issue362, just a minor annoyance along its way. (issue362, Eric Kow)

  * Get 'open' and 'psignal' declared on Solaris. (Dave Love)

  * Zsh completion supports new _darcs/pristine repository format. (Georg Neis)

  * Add documentation for DARCS_PAGER. (Benedikt Schmidt)

  * Turning off and on the ssh control master works for the Changes command.
    (issue383, Georg Neis)

  * Optimize unrecorded file moves with unrecorded file adds and removals.
    That is, if you add, rename and remove files multiple times without
    recording, whatsnew (and record) will only see the final result, not the
    whole sequence of moves. (Marco Tulio Gontijo e Silva)

  * Fix link error with errno for gcc 4.12 / glibc 2.4. (Benedikt Schmidt)

  * Remove the confusing text "user error" from some of GHC's error
    descriptions. (Juliusz Chroboczek)

  * Check for and fail build configuration if module quickcheck isn't
    available. (issue369, David Roundy)

  * Make darcs push QP-encode the bundle before transferring. This should
    hopefully fix issues with scp/sftp corrupting bundles in transit. (Juliusz
    Chroboczek)

  * Make it very clear in the documentation that the options --from and
    --author does NOT have anything to do with the sender or email author when
    sending patches as email with the darcs Send command. (Kirsten Chevalier)

  * Allow commented tests in tests_to_run. (David Roundy)

  * Make it an error to Put into a preexisting directory. Often one could be
    tempted to try to put into a directory, expecting to have the repository
    created as a subdirectory there, and it is confusing to have instead the
    repository contents mingled with whatever was already in that directory.
    (David Roundy)

  * Explicitly flush output on Windows after waiting for a lock, because
    Windows' stdout isn't always in line-buffered mode. (Simon Marlow)

  * Improve unhelpful "fromJust" error message in Push command. (Kirsten
    Chevalier)

  * Support option --all for Obliterate, Unpull and Unrecord. (issue111, David
    Roundy)

  * Ignore failure to set buffering mode for terminal in some places
    (supposedly fixes issue41, issue94, issue146 and issue318). (Tommy
    Pettersson)

  * Buildfix, don't import Control.Exception functions when compiling on
    Windows. (Edwin Thomson)

  * Add make rules for tag files. (Dave Love)

  * Add a semi-automated test for SSH-related things. (Eric Kow)

  * Allow Dist --dist-name to put the tar file in any directory by giving a
    full path as the dist name. (issue323, Wim Lewis)

  * Add rigorous error checking when darcs executes external commands. All
    low-level C return values are checked and turned into exceptions if they
    are error codes. In darcs main ExecExceptions are caught and turned into
    error messages to help the user. (Magnus Jonsson)

  * Redirect error messages from some external commands to stderr. (Tommy
    Pettersson)

  * Make configure fail if a required module is missing. (David Roundy)

  * The options for turning off and on the ssh control master works from the
    defaults file. (issue351, Tommy Pettersson)

  * Amend-record now keeps explicit dependencies (made with --ask-deps) from
    the amended patch. (issue328, Edwin Thomson)

  * Make libcurl use any http authentication. This let darcs use repositories
    protected with digest authentication. (Tobias Gruetzmacher)

  * Turning off and on the ssh control master works for the Send command.
    (Eric Kow)

  * Redirect stderr to Null when exiting SSH control master. This suppresses
    the output "Exit request sent" not suppressed by the quiet flag. (Eric Kow)

  * Fix curses stuff, especially on Solaris 10. (Dave Love)

  * Annotate various boring patterns. (Dave Love)

 -- Tommy Pettersson <ptp@lysator.liu.se>  Sun, 03 Jun 2007 21:37:06 GMT

darcs (1.0.9rc2)

  * Pass e-mail address only for %t in --sendmail-command. Msmtp seems to
    require this. Note that the full address is encoded in the message body.
    (Eric Kow)

  * Show error messages on stderr when starting and stopping the ssh control
    master. (Tommy Pettersson)

  * Rewrite check for spoofed patches with malicious paths. The check can now
    be turned off with the option --dont-restrict-paths (issue177). The new
    check only works for Apply and Pull, and it only looks at the remote
    patches. A more complete check is desirable. (Tommy Pettersson)

  * Add LGPL file referenced in fpstring.c (Dave Love).

  * Update FSF address in copyright headers(Dave Love).

  * New default boring file patterns: ,v .# .elc tags SCCS config.log .rej
    .bzr core .obj .a .exe .so .lo .la .darcs-temp-mail .depend and some more
    (Dave Love).

  * Move darcs.ps to the manual directory (Tommy Pettersson).

  * Pass -q flag to scp only, not ssh and scp. Putty's SSH (plink) does not
    recognize the -q flag. (issue334, Eric Kow)

  * Bugfix. Make darcs.cgi look for both pristine and current (Dan).

  * Don't lock the repo during `query manifest' (issue315, Dave Love).

  * Buildfix. Include curses.h with term.h (issue326, Dave Love).

  * Bugfix. Unrecord, Unpull and Obliterate could mess up a repository
    slightly if they removed a tag with a corresponding checkpoint. Only the
    commands Check and Repair were affected by the damage, and Get would also
    copy the damage to the new repository. (issue281, Tommy Pettersson)

  * Add a HACKING file with helpful references to pages on the darcs wiki
    (Jason Dagit).

  * New boring file patterns: hi-boot o-boot (Bulat Ziganshin, Eric Kow).

  * Require 'permission denied' test for MacOS X again. Perhaps something in
    MacOS X was fixed? (Eric Kow).

  * Look for Text.Regex in package regex-compat. Needed for GHC 6.6. (Josef
    Svenningsson)

 -- Tommy Pettersson <ptp@lysator.liu.se>  Sun, 16 Nov 2006 14:03:51 GMT

darcs (1.0.9rc1)

  * Improved handling of input, output and error output of external commands.
    Null-redirection on windows now works. Only stderr of ssh is
    null-redirected since putty needs stdin and stdout. (issue219, Eric Kow,
    Tommy Pettersson, Esa Ilari Vuokko)

  * Optimize away reading of non-managed files in summary mode of Whatsnew
    --look-for-adds (issue79, Jason Dagit).

  * Remove direct dependency to mapi32.dll; Improve MAPI compatibility. (Esa
    Ilari Vuokko)

  * Ignore .git if _darcs is found (Juliusz Chroboczek).

  * Add a haskell code policy test to catch uses of unwanted functions, bad
    formating and such. (Tommy Pettersson)

  * If the logfile supplied with option --logfile does not exist, fail instead
    of inserting no long comment. (issue142, Zachary P. Landau)

  * Make the pull 'permission test' work when run as root (Jon Olsson).

  * Handle unsimplified patches when further simplifying the summarized
    output. For unknown reason (a possibly previous version of) darcs allows a
    single patch to Add and Remove the same file in a single patch. The
    Changes command used to combine them, showing just a Remove. (issue185,
    Lele Gaifax)

  * Add workaround for HasBounds that was removed in GHC 6.6 (Esa Ilari
    Vuokko).

  * Really make --disable-ssh-cm work (issue239, Eric Kow).

  * Fix false errors in pull.pl test (David Roundy).

  * Clean up docs on DarcsRepo format (David Roundy).

  * Use stdin for passing the batch file to sftp, to allow password-based
    authentication (issue237, Eric Kow, Ori Avtalion).

  * Make darcs fail if the replace token pattern contains spaces. It would
    otherwise create a non-parsable patch in pending. (issue231, Tommy
    Pettersson)

  * Set a default author in the test suite harness so not every test has to do
    so. (Tommy Pettersson).

  * Run external ssh and scp commands quietly (with the quiet flag), but not
    sftp which doesn't recognize it (issue240). This reduces the amount of
    bogus error messages from putty. (Eric Kow)

  * Implement help --match, which lists all available forms for matching
    patches and tags with the various match options (Eric Kow).

  * Added .elc and .pyc suffixes to default binary file patterns (Juliusz
    Chroboczek ).

  * Added a link to the 'projects' part of the cgi repository interface, so
    that you go back to the project list (Peter Stuifzand).

  * Add a test suite for calling external programs (Eric Kow).

  * Don't warn about non-empty dirs when in quiet mode (Eric Kow).

  * New option --umask. This is best used in a repository's defaults file to
    ensure newly created files in the repository are (not) readable by other
    users. It can also be used when invoking darcs from a mail reader that
    otherwise sets a too restrictive umask. (Issue50, Juliusz Chroboczek)

  * Only check for ssh control master when it might be used. This suppresses
    the annoying "invalid command" error message. (Issue171, Eric Kow)

  * Fail with a sensible message when there is no default repository to pull
    from. (Lele Gaifax)

 -- Tommy Pettersson <ptp@lysator.liu.se>  Sun, 08 Oct 2006 17:52:07 GMT

darcs (1.0.7)

  * Fixed bug leading to a spurious "darcs failed: resource vanished" error
    message when darcs output is piped to a program such as head that then
    exits. (Issue160, David Roundy)

  * New option --diff-command overrides the default value of "diff" when darcs
    calls an external program to show differences between versions (Eric Kow).

  * Use the ControlMaster feature in OpenSSH version 3.9 and above to
    multiplex ssh sessions over a single connection, instead of opening a new
    connection for every patch (Issue32, Eric Kow).

  * Add a standalone graphical interface (experimental). The gui code prior to
    this patch allows graphical darcs forms to be run from the command line.
    This builds off that functionality by adding a graphical front-end,
    allowing users to access these forms with a click of a button. In other
    words, this allows users to run darcs without the command line. (Eric Kow)

  * Make unpull, unrecord, obliterate accept --gui (Eric Kow).

  * Freshen GUI code so that it compiles (Eric Kow).

  * Provide more information when a remote repository can't be correctly
    identified. (Juliusz Chroboczek)

  * The Send command can save, reuse and delete the accompanying description
    in a logfile. (Zachary P. Landau)

  * Display list of subcommands when getting help on a supercommand. (Eric Kow)

  * A proper fix for the problem with rmdir when there are non-managed files
    left in the working copy of the directory so it can't really be removed.
    This solves the two related problems with a missguiding error message in
    one case, and an unreported repository corruption in the other. Now there
    is no false warning and no repository coruption. (issue154, Eric Kow)

  * Escaping of trailing spaces and coloring now works with in the pager
    called with 'p' from interactive dialogues. (issue108, Tommy Pettersson)

  * Added default recognized binary file extensions: bmp, mng, pbm, pgm, pnm,
    ppm, tif, tiff. (Daniel Freedman)

  * Added a RSS link to common.xslt. (Peter Stuifzand)

  * Make short option -i a synonym for --interactive (Zachary P. Landau).

  * Improved argument substitution for --external-merger. All apperences of %N
    are replaced, not only those occurring as single words. (Daan Leijen)

  * Transition from _darcs/current to _darcs/pristine completed. New
    repositories are created with a "pristine" directory. Previous versions of
    darcs have been looking for this directory since version 1.0.2, but older
    versions than that can't read the new repository format. (Juliusz
    Chroboczek)

  * If you specify a repository directory, any absolute paths prefixed by this
    directory are converted to be ones relative to the repodir. (issue39, Eric
    Kow)

  * The --repodir flag works with many more commands: changes, dist, get,
    optimize, repair, replace, setpref, tag, trackdown. (RT#196, RT#567, Eric
    Kow)

  * The --repodir flag works with initialize command, and tries to create it
    if it does not exists. (RT#104, Eric Kow)

  * Add autom4te.cache to default boring patterns. (Kirill Smelkov)

  * Don't create temporary copies of the repository for the external merger
    program, unless there is for sure some conflict to resolve. (Edwin Thomson)

  * Modify Changes --interactive dialogue to behave like other interactive
    commands: accept 'y' and 'n' as answers and exit automatically after last
    question. (Zachary P. Landau)

  * Unnamed patches are now called "changes" in the interactive patch
    selection dialogues. (Tommy Pettersson)

  * Treat Enter as an invalid response in single character prompt mode, and
    give feedback instead of being mysteriously silent and unresponsive.
    (RT#261, Eric Kow)

  * Make short option -f a synonym for --force (Zooko).

  * Posthooks no longer cause an output message about success or failure,
    unless the --verbose option is used. (Jason Dagit)

  * Fix crash when using changes --interactive with --patch or --match
    (Zachary P. Landau).

 -- Tommy Pettersson <ptp@lysator.liu.se>  Sun, 13 May 2006 17:14:38 GMT

darcs (1.0.6)

 -- Tommy Pettersson <ptp@lysator.liu.se>  Sun, 28 Feb 2006 11:18:41 GMT

darcs (1.0.6rc1)

  * Check paths when applying patches to files and directories to stop
    maliciously handcrafted patches from modifying files outside of the
    repository or inside darcs private directories (issue48, Tommy Pettersson).

  * Revert optimization that sometimes applied patches incorrectly and
    corrupted the repository. This make darcs somewhat slower when applying
    patches. A full pull of the darcs repository itself takes 50% longer.
    (issue128, Tommy Pettersson).

  * Fix bug in Get --tag that produced a corrupt repository (issue67, Edwin
    Thomson).

  * Add newline between long comment and changed files list in dry-run summary
    to remove ambiguity (Edwin Thomson).

  * Extended date matching functionality: ISO 8601 dates and intervals, a
    larger subset of English like "yesterday at noon" (issue31/RT#34, Eric
    Kow).

  * Allow rename to different case (RT #466, Eric Kow).

  * Save long comment in a file if record fails the test run (Zachary P.
    Landau).

  * Fix win32 build breaks (Will Glozer).

  * Make --exact-version work when darcs is built from distributed tar ball
    (Marnix Klooster).

  * Coalesce pending changes created with setpref (issue70/RT#349, Eric Kow).

  * Support --interactive option in changes command (issue59, Zachary P.
    Landau).

  * New help command (RT#307, Eric Kow).

  * Add --without-docs option to configure (Richard Smith).

  * Obey normal autoconf conventions. Allows you to 'make install prefix=...'
    and doesn't change default for sysconfdir. (Dave Love)

  * Fix bug with non-existing directories. (David Roundy)

  * Remote apply does not use cd to change current directory to target
    directory any more. It uses --repodir when invoking remote darcs. This may
    break some darcs wrappers. (Victor Hugo Borja Rodriguez)

  * Support signed push (Esa Ilari Vuokko).

  * Added support for pulling from multiple repositories with one pull. The
    choice of --union/--intersection determines whether all new patches are
    pulled, or just those which are in all source repositories. This feature
    implements a suggestion by Junio Hamano of git. (David Roundy)

  * Patch bundle attachments get a file name, based on the first patch.
    (Zachary P. Landau)

  * The send command now takes a --subject flag. (Joeri van Ruth)

  * Fix --set-scripts-executable to work also when getting a local repository.
    (issue38, Eric Kow)

  * Removed the helper program darcs-createrepo. It was used for guided settup
    of a darcs repository and a corresponding user account to accept patches
    from signed emails. (issue14, Jason Dagit)

  * Print out the patch name when a test fails. (Zachary P. Landau).

  * Bugfix for --sendmail-command in Windows (Esa Ilari Vuokko).

  * Make apply --verify work with GnuPG in Windows (Esa Ilari Vuokko)

  * Bugfix for handling of absolute paths in Windows (issue47, Will Glozer)

 -- Tommy Pettersson <ptp@lysator.liu.se>  Sun, 19 Feb 2006 23:19:19 GMT

darcs (1.0.5)

  * Fixes for Windows (Will Glozer).

  * Adapt makefile to work with current ghc 6.4 (Will Glozer).

  * --help and --list-commands ignore other options (issue34, Eric Kow).

  * Fix apply with --verify for patch bundles signed by GnuPG in Windows (Esa
    Ilari Vuokko).

  * Make patch selection options together with --context work again (Daniel
    Bünzli).

  * Make option --commands outside of a repository work again (issue9, David
    Roundy).

  * Bugfix for pushing with ssh under Windows (issue15, Will Glozer).

  * Fix superfluous input bug in test suite (Florian Weimer).

  * Many English and markup fixes (Dave Love).

 -- Tommy Pettersson <ptp@lysator.liu.se>  Sun, 07 Dec 2005 11:27:30 GMT

darcs (1.0.4)

  * Fixed a bug in the external conflict resolution code. (bug #577, David
    Roundy)

  * Fixed a bug which made apply sometimes (but rarely) fail to check the the
    hash on patch bundles corrupted in just the wrong way. (David Roundy)

  * Added a simple check to darcs replace that avoids tokenizing lines that
    don't contain the token we're replacing. I feel a bit bad about
    introducing an optimization this late in the release cycle, but it makes a
    huge difference, and really should be safe. (David Roundy---famous last
    words)

  * Fixed bug where darcs didn't honor the SSH_PORT environment variable when
    calling sftp. (bug #576, fix suggested by Nicolas Frisby)

  * Avoid putting a wrongly-named directory in the tarball generated by darcs
    dist, if the name we wanted already exists in $TMPDIR. (Simon McVittie)

  * Fixed bug which caused "pull_firsts_middles called badly" errors when
    running record with --ask-deps flag. (bug #476, David Roundy)

  * Fixed bug where 'darcs changes --context' created a context that contained
    escapes that prevented its use with send. (bug #544, David Roundy)

  * Make interactive push/pull/send/apply respect the --summary option by
    printing each patch as if you had hit 'x'. (David Roundy, bug #512)

  * Fix bug when calling whatsnew --summary when a file without a trailing
    newline has been deleted. (David Roundy)

  * Fix --set-scripts-executable to work again. This feature had been broken
    since version 1.0.3. (David Roundy)

  * Simple (safe) fix for a bug which caused darcs to run out of file
    descriptors when pulling over 1000 patches. (David Roundy)

  * Fix bug in perl parts of test suite which led to spurious warning
    messages. (David Roundy)

  * Fix bug in configure when compiling darcs from tarball on a system that
    has no darcs present. (David Roundy)

  * Fix bug that shows up when recording in a repository lacking a pristine
    tree. (David Roundy)

 -- David Roundy <droundy@darcs.net>  Sun, 13 Nov 2005 13:44:31 GMT

darcs (1.0.4pre4)

  * Fix error in install target of makefile, which was introduced in
    1.0.4pre3. (Andres Loeh)

  * Fix problem where make install modified the permissions on system
    directories. (David Roundy, bug #494)

  * Fix bug in display when whatsnew is given "-l" twice. (David Roundy, bug
    #501)

  * Added support for --posthook to all commands. (Jason Dagit)

  * Made repair able to work on partial repositories. (fixes bug #189)

  * Changed the delimiter in the long-comment file from ***DARCS*** to ***END
    OF DESCRIPTION*** and clarified its meaning a bit. (Jason Dagit and David
    Roundy)

  * Added code to allow darcs to apply some patch bundles that have had
    carriage returns added to their line endings. (David Roundy, bug #291)

  * Make darcs accept command line flags in any order, rather than requiring
    that they precede file, directory or repository arguments. Fixes bug #477
    (David Roundy)

  * Modified darcs get to display patch numbers rather than a bunch of dots
    while applying patches during a darcs get. Also added similar feedback to
    the check and repair commands. (Mat Lavin, bug #212)

  * Made revert --all not ask for confirmation, so it can be used in
    scripting, as one would use pull --all or record --all. (Jani Monoses)

  * Added file ChangeLog.README explaining how to add entries to the
    changelog. (Mark Stosberg and David Roundy)

  * Fixed incompatibility with somewhat older versions of libcurl. (Kannan
    Goundan)

  * Fixed bug that showed up when after editing a long comment, the long
    comment file is empty. (David Roundy, bug #224)

 -- David Roundy <droundy@darcs.net>  Sun, 01 Sep 2005 11:04:18 GMT

darcs (1.0.4pre2)

  * (EXPERIMENTAL) Added support for reading and writing to git repositories.
    There remain efficiency issues with the handling of git merges and darcs
    is not yet able to either create a new git repository, or to pull from a
    remote git repository. See building darcs chapter in manual for
    instructions on building darcs with git support. (Juliusz Chroboczek,
    configuration contributed by Wim Lewis)

  * Add new "query manifest" command to list files and/or directories in
    repository. Add some related infrastucture to support "subcommands".
    (Florian Weimer)

  * Make configure properly detect that we're on windows when building under
    mingw. (David Roundy)

  * Fixed bug #208: error when specifying pulling from a relative default
    repository if we are ourselves within a subdirectory of our repository.
    (David Roundy)

  * Change internal mechanism for writing the "pending" file to be (hopefully)
    more robust. (David Roundy, original idea by Ian Lynagh)

  * Fixed a bug that caused get --partial to fail sometimes. (David Roundy)

  * Made push/pull --verbose --dry-run now display contents of patches,
    analogous to the behavior of changes --verbose. (Jim Radford)

  * Various build system cleanups. (Peter Simons)

 -- David Roundy <droundy@abridgegame.org>  Sun, 31 Jul 2005 12:10:29 GMT

darcs (1.0.4pre1)

  * Performance improvement: Several commands now read patches lazily, which
    reduces memory usage. A test of 'darcs check' on the Linux kernel
    repository showed the memory usage was nearly cut in half, from about 700
    Megs to 400. Coded by David Roundy.

  * New feature: darcs put, the easiest way to create a remote repo, symmetric
    with 'darcs get'. Coded by Josef Svenningsson.

  * Performance improvement: RT#222: darcs performs better on files with
    massive changes. Coded by Benedikt Schmidt.

  * New Feature: darcs optimize now has "--modernize-patches" and
    "--reorder-patches" flags. See the manual for details.

  * Performance improvement: Using 'darcs diff' is now exponentially faster
    when comparing specific files in the working directory to the most recent
    copy in the repo. Coded by kannan@cakoose.com.

 -- David Roundy <droundy@abridgegame.org>  Sun, 18 Jul 2005 11:22:34 GMT

darcs (1.0.3)

  * Fixed bug #396: error when specifying a removed file on the command line
    of darcs record.

 -- Tomasz Zielonka <tomasz.zielonka@gmail.com>  Sun, 24 May 2005 21:51:27 GMT

darcs (1.0.3rc2)

  * Internals: darcs' ChangeLog is automatically generated from repo history
    and a database of ChangeLog entries (Tomasz Zielonka)

  * Fixed: RT#370: --tags work in unpull and unrecord (Tommy Pettersson)

  * New feature: added support for displaying patches with pager when
    selecting patches (Benedikt Schmidt)

  * New feature: new match type "exact" (John Goerzen)

  * Feature: unrevert accepts --all and --interactive options (Jani Monoses)

  * Fixed: darcs works with nvi (Benedikt Schmidt)

 -- Tomasz Zielonka <tomasz.zielonka@gmail.com>  Sun, 15 May 2005 08:56:17 GMT

darcs (1.0.3rc1)

  * New Feature: darcs.cgi now uses appropriate caching headers. This will
    make repeated calls to the same pages by cache-aware browsers much faste
    in some cases. It also means that darcs.cgi can be usefully combined with
    a cache-aware proxy for even better performance. (Will Glozer)

  * New feature: more control over color and escaping in printed patches,
    alternative color scheme, escaping of trailing spaces (Tommy Pettersson)

  * Fixed: fixed bug manifesting with failed createDirectory (David Roundy)

  * Internals: RT#255, several welcome refactors were made to the test suite,
    including comprehensible shell test script output, improved portability,
    and easier maintenance. (Michael Schwern).

  * New Feature: RT#245: Using --look-for-adds with 'whatsnew' implies
    --summary. This should save some typing for the common case. (Karel
    Gardas, Mark Stosberg)

  * New Feature: RT#231: darcs gives better feedback now if you try to record
    nonexistent files or directories. (Karel Gardas, Mark Stosberg)

  * New feature: send accepts --sendmail-command that allows to customize the
    command used for sending patch bundles (Benedikt Schmidt)

  * Fixed: RT#266: Adding a non-existent dir and file gives the expected
    message now. (Tomasz Zielonka).

  * Fixed: RT#10, a missed conflict resolution case. More accurately, we
    noticed at had been fixed some point. A regression test for it was added.
    (Tomasz Zielonka, Mark Stosberg)

  * New feature: darcs tag can now accept the tag name on the command line
    (RT#143). (Josef Svenningsson, Mark Stosberg, David Roundy)

  * New feature: unrecord and unpull have a more powerful interface similar to
    'darcs pull'. This allows for multiple patch selection. Coded by Tommy
    Pettersson.

  * Bug fix: RT#305: Removed '--patch' from the 'changes', which conflicted
    with the new '--patches' option.

  * New feature: Automatically add parent directories for darcs add. (RT#20)
    Coded by Benedikt Schmidt.

  * Add helpful diagnostic message when there is a failure while pulling
    (RT#201)

 -- Tomasz Zielonka <tomasz.zielonka@gmail.com>  Sun, 26 Apr 2005 00:25:54 GMT

darcs (1.0.2)

  * No changes from 1.0.2rc4.

 -- David Roundy <droundy@darcs.net>  Fri,  4 Feb 2005 07:33:09 -0500

darcs (1.0.2rc4)

  * More documentation improvements, plus one clearer error message.
  * Fixed (new since 1.0.1) bug which broke darcs repair.
  * Fixed problem with makefile which caused spurious relinkings.
  * Fixed bug in new optimize --relink command, which could cause
    repository corruption.

 -- David Roundy <droundy@abridgegame.org>  Wed, 2 Feb 2005 06:24:19 -0500

darcs (1.0.2rc3)

  * Documentation improvements related to Juliusz new code.
  * Fixed longstanding leaks in zlib/threads code.
  * Fixed some bugs in the new optimize --relink code.
  * Fixed bug in darcs diff when the repository name is empty.

 -- David Roundy <droundy@abridgegame.org>  Sat, 29 Jan 2005 07:28:39 -0500

darcs (1.0.2rc2)

  * Fixed bug on win32 when there are spaces in a repositories path and an
    external program (i.e. ssh) is called. (Will Glozer)

 -- David Roundy <droundy@abridgegame.org>  Thu, 27 Jan 2005 06:46:37 -0500

darcs (1.0.2rc1)

  * Added experimental support for repositories without a "pristine tree"
    This is the new name for the cache stored in _darcs/current/.
    (Juliusz Chroboczek)
  * Added an optimize --relink command to save disk space when using
    multiple repositories. (Juliusz Chroboczek)
  * Ignore conflict markers in the boring and binaries files.
  * Fixed bug in get --partial when patches are in an unusual order.
    (Andrew Johnson)
  * Fixed bug which caused a crash on a local get of a repository owned by
    another user.
  * Fixed bug in changes/annotate that shows up when a directory has been
    moved.
  * Allow ncurses in addition to curses in configure.
  * Added --set-scripts-executable option. (Karel Gardas)
  * Added configure option to fix path to sendmail even if it's not
    present.
  * Made bash completion more robust regarding shell special chars.
  * Added konquerer workaround to cgi annotate. (Will Glozer)
  * Addressed bug #114 - provide a better error when you accidently try to
    pull from yourself. (Mark Stosberg)
  * Made a few documentation improvements.
  * Made http user agent reflect the darcs and libcurl version.
  * Fixed commute bug in merger code.
  * Fixed bug in decoding mime messages.

 -- David Roundy <droundy@abridgegame.org>  Wed, 26 Jan 2005 08:51:24 -0500

darcs (1.0.1)

  * Made darcs changes --context work on an empty repo.
  * Fixed bug in relative directories as arguments to pull/push.
  * Fixed bug leading to extraneous changes in pending.
  * Fixed bug #137 - XML escaping for >.
  * Fixed gui code to compile with wxhaskell 0.8 (but it's still buggy).

 -- David Roundy <droundy@abridgegame.org>  Tue, 14 Dec 2004 08:16:10 -0500

darcs (1.0.1rc3)

  * Made it so adding and removing a file doesn't leave changes in pending.
  * Fixed bug in creating the file to be edited for the long comment.
  * Made "bug in get_extra" message explain possible cause of the problem,
    which is related to a bug in commutation that made it into version
    1.0.0.
  * Fixed stubborn bug in annotate.
  * Fixed problem when unrecording binary file patches.

 -- David Roundy <droundy@abridgegame.org>  Sat, 11 Dec 2004 14:23:53 -0500

darcs (1.0.1rc2)

  * Various optimizations.
  * darcs now supports an arbitrary number of transport protocols through the
    use new environment variables. See DARCS_GET_FOO in the 'Configuring
    Darcs' chapter in the manual for more details.
  * darcs now supports an arbitrary number of concurrent connections when
    communicating with remote repos. See the documentation for DARCS_MGET_FOO
    in the 'Configuring Darcs' chapter in the manual for more details.

 -- David Roundy <droundy@abridgegame.org>  Wed,  8 Dec 2004 08:02:48 -0500

darcs (1.0.1rc1)

  * Fixed bug in commutation of adjacent hunks which have either no new or
    no old lines.
  * Numerous newline fixes for windows.
  * On windows, use MAPI to resolve to and from addresses.
  * Fixed problem where the --cc was ignored in apply if the patch
    succeeded.

 -- David Roundy <droundy@abridgegame.org>  Wed,  1 Dec 2004 06:24:08 -0500

darcs (1.0.1pre1)

  * Changed apply to by default refuse to apply patches that would lead to
    conflicts.
  * Removed the old darcs_cgi script, in favor of the darcs.cgi script.
  * Fixed changes to work better in partial repositories.
  * Set stdin and stdout to binary mode to fix end of line problems with
    push under windows.
  * Made send create proper MIME email.
  * Removed reportbug command, really wasn't necesary, and didn't work
    well.  Report bugs by an email to bugs@darcs.net, which creates a
    ticket in our BTS.
  * Allow darcs to work with a password protected proxy.
  * Get multiple files with a single wget call when darcs is compiled
    without libcurl support.
  * Use sftp instead of scp to copy patches via ssh -- this reuses a single
    connection for better speed.
  * Made _darcs/current polymorphic (but not really documented).
  * Made optimize --uncompress work with --partial repos.
  * Various minor interface improvements.
  * Made changes work better when specifying a file, and working in a
    partial repository.
  * Fixed bug in causing "Fail: _darcs/patches/unrevert: removeFile: does
    not exist (No such file or directory)".  Resolves bugs #57, #61.

 -- David Roundy <droundy@abridgegame.org>  Sun, 21 Nov 2004 08:29:24 -0500

darcs (1.0.0)

  * Fixed compile error when antimemoize is enabled.
  * Fixed bug that showed up when dealing with international characters in
    filenames.
  * Various documentation improvements.

 -- David Roundy <droundy@abridgegame.org>  Mon,  8 Nov 2004 06:12:08 -0500

darcs (1.0.0rc4)

  * Use autoconf to check for working install program.
  * Renamed rerecord to amend-record in a futile attempt to avoid
    confusion.
  * Made pull accept the --repodir option.
  * Fixed off-by-one error in annotate that kept users from seeing
    "deleted" version.
  * Check filesystem semantics at runtime to avoid using mmap on
    windows filesystems.
  * Fixed darcs.cgi to work properly when browsing history of renamed
    files.
  * Use anonymous file handle for temporary files in darcs.cgi -- fixes a
    temporary file leak and potentially improves security.
  * Added --summary option to commands that accept --dry-run.
  * Made pull prompt for confirmation when there is a conflict with
    unrecorded changes.
  * Made unrevert interactive.
  * Don't try to generate a new name on get if name was given explicitely.
  * Always mark conflicts, even if there's an obvious solution.
  * Quote conflict attribute values in xml output.
  * Fail if the user gives a newline in the patch name.
  * Fixed bug where new files didn't show up in darcs diff.
  * Really fix newlines in whatsnew -u.
  * Fixed bug in handling of tags in changes and annotate.
  * Fixed bug in default options containing "--".
  * Fixed various other build problems in 1.0.0rc3.
  * Fixed embarrassing failure-to-compile-on-windows bug.

 -- David Roundy <droundy@abridgegame.org>  Mon,  1 Nov 2004 05:19:01 -0500

darcs (1.0.0rc3)

  * Fixed bug leading to creation of empty "hunk" patches.
  * Fixed bug in rollback when there are pending changes.
  * Fixed push bug when default apply is --interactive.
  * Fixed a bug where "darcs pull --list-options" would try to
    complete to "home/.../darcs_repo" instead of "/home/.../darcs_repo".
  * Fixed flushing bug in darcs.cgi.
  * Fixed commutation bug with renames and file adds/removals.
  * Made --summary indicate conflicted changes.
  * Fixed generation of extra hunk in diff algorithm.
  * Added X-Mail-Originator header to emails sent by darcs.
  * Fixed a couple of bugs in the resolve command.
  * Added new cgi diff command to produce unified diff.
  * Notify when there are conflicts on push.
  * Added 'a' key to say yes to all remaining changes for interactive
    commands.
  * Automatically generate AUTHORS file from repo history.
  * Made pull --quiet actually quiet.
  * Fixed bugs in whatsnew -ls, and distinguished between manually added
    files and automatically added files.
  * Fixed bug in darcs --commands when called outside a repo.

 -- David Roundy <droundy@abridgegame.org>  Sun,  3 Oct 2004 07:45:05 -0400

darcs (1.0.0rc2)

  * Added support for comments in prefs files.
  * Added new --enable-antimemoize compile option which reduces memory
    usage at the expense of increased computational time.
  * Added a new command:  "reportbug"
  * Fixed a bug that prevented applying of a patch bundle to an
    "unoptimized" repo.
  * Fixed bug where asking for changes to a nonexistent file in a
    subdirectory would show the patch that created or renamed that
    subdirectory.
  * Improved the robustness of unrevert.  Now actions that will make
    unrevert impossible should warn the user.
  * Fixed bug when moving files or directories to the root directory of
    repo.
  * Various changes to make the --logfile way of specifying the patch name
    and comments in record more friendly:
    - Allows editing of the long comment even when --logfile is specified,
      if the --edit-long-comment option is also used.
    - When editing the long comment, the change summary is included below
      the actual text for reference, and the patch name is included in the
      first line (and thus may be modified).
    - The --logfile option is ignored if such a file doesn't exist.
    - A --delete-logfile option was added, which tells darcs to delete the
      file after doing the record.  This is intended to allow you to stick
      a --logfile=foo option in your defaults without accidentally
      recording multiple patches with the same comments because you forgot
      to modify it.
  * Fixed bug leading to .hi files in tarball.
  * Made ctrl-C work under windows, but only "pure" windows consoles, not
    in cygwin shells or mingw's rxvt (room for improvement here).
  * Fixed bug that led to curl not being tried when darcs is not compiled
    with libcurl.
  * Added an environment variable DARCS_USE_ISPRINT for people who use
    non-ascii characters in their files to indicate if the haskell standard
    library "isPrint" function works properly on their system (which
    depends on locale).
  * Reduced the number of hunks produced by the internal diff algorithm,
    when there are multiple equivalent ways of interpreting a change.
  * Made the --from-{patch,tag,match} options inclusive, and added a
    --{patch,match} option to diff (which was made easier to define by the
    inclusiveness change, since --patch x is now equivalent to
    --from-patch x --to-patch x).
  * Added support for a second argument to get, which specifies the name of
    the new directory.

 -- David Roundy <droundy@abridgegame.org>  Sun, 12 Sep 2004 06:54:45 -0400

darcs (1.0.0rc1)

  * Remove some lazy file IO which may have been causing trouble pushing in
    windows and using windows shares.
  * Various interface improvements and improved error messages.
  * Fixed bug that could cause conflicts in pending when unrecording a
    patch that contained two non-commuting non-hunk patches.
  * Fixed bug in --ask-deps option of record.
  * Added --exact-version option which gives the precise darcs context from
    which darcs was compiled.
  * MIME fixes in patch forwarding.
  * Various improvements to the darcs.cgi script.
  * Added --reverse option to changes.
  * Fixed patch numbering when file or directory arguments are given to an
    interactive command.

 -- David Roundy <droundy@abridgegame.org>  Sun, 15 Aug 2004 07:43:30 -0400

darcs (0.9.23)

  * Added a rerecord command, which will add changes to an existing
    recorded patch
  * Added support for a MOTD.
  * Vastly improved the speed and memory use of darcs optimize --checkpoint
    as well as darcs record, in the case where a record consists primarily
    of adds.

 -- David Roundy <droundy@abridgegame.org>  Mon, 26 Jul 2004 08:11:20 -0400

darcs (0.9.22)

  * add preliminary --context option to changes and get.
  * display change number, e.g. "(1/3)" in interactive commands.
  * show moves in summary format.
  * add hash of patch bundles in darcs send.
  * properly support --verbose and --quiet in add.
  * don't display binary data to screen.
  * fix bug in selecting patches by pattern.
  * fix various locking-related bugs introduced in 0.9.21.
  * fix bug when specifying logfile in a subdirectory.
  * support backslashes for directory separators in windows.
  * fix file modification time bug.

 -- David Roundy <droundy@abridgegame.org>  Sat, 26 Jun 2004 07:42:05 -0400

darcs (0.9.21)

  * made mv work even if you've already mv'ed the file or directory.
  * remember configure settings when reconfiguring.
  * added --leave-test-directory to save the directory in which the test is
    run on record or check.
  * added HTTP redirect support (thanks Benedikt).
  * fixed problems when unrecording a patch with conflits.
  * fixed locking on nfs (thanks Juliusz).
  * added preliminary version of a new cgi script for browsing darcs
    repositories (thanks to Will Glozer for contributing this).
  * add and modify a number of short flag options.
  * fix bug in applying new order patch bundles that are GPG signed.
  * fix bug in diff when a tagged version was requested.

 -- David Roundy <droundy@abridgegame.org>  Sat, 12 Jun 2004 05:39:48 -0400

darcs (0.9.20)

  * fix bug in darcs-createrepo.
  * add support for DARCS_SCP and DARCS_SSH environment variables.
  * add XML support for --summary options of changes and annotate.
  * better command-line completion on commands accepting a list of files or
    directories.
  * fix bug causing empty hunk patches to lead to failures.
  * fix bug where --all overrode file choice in record.
  * fix bug when testing patches that create subdirectories within subdirectories.
  * preserve pending changes when pulling or applying.
  * give better error message in pull when patch isn't readable.
  * allow sendEmail with no "to", just "cc" recipients.  This should fix
    the trouble with trying to --reply to a patch coming from a push rather
    than a send.

 -- David Roundy <droundy@abridgegame.org>  Wed,  5 May 2004 06:01:48 -0400

darcs (0.9.19)

  * fix bugs leading to failures in the wxhaskell interface.
  * fix bug that caused darcs diff to fail.
  * fixed bug in get that lead to _darcs/current/_darcs directories.
  * improved error reporting in several situations.
  * fixed bug when pulling or pushing into an empty repo.
  * added --summary option to changes to summarize the changes made in each
    patch.

 -- David Roundy <droundy@abridgegame.org>  Fri,  9 Apr 2004 07:19:34 -0400

darcs (0.9.18)

  * added support for sending email from windows using the MAPI interface.
    This code attaches the patch bundle in base64-encoded form, which darcs
    can't currently decode (expect that in the next release), but the patch
    bundle can be manually applied if a mail program does the decoding.
  * renamed "darcs push" to "darcs send" and added a new "darcs push"
    command roughly equivalent to the old "darcs push --and-apply".
  * removed support for setting up a test suite by simple creating a file
    named "darcs_test".  You now should use setpref to define the test
    suite command.
  * fixed some problems when working in a --partial repository.
  * lots of code was cleaned up.  We have enabled the -Wall compiler flag
    and are in the process of eliminating all the warnings.  This should
    make the code more friendly to new developers, and also helps with the
    next bullet point:
  * improved handling of errors--informative failure messages are more
    likely than they were before.
  * by default only check changes made since last checkpoint--this greatly
    speeds up check.
  * add --quiet option.  Some commands don't yet support this.  If
    there's a command you want to quiet down, let us know.
  * several performance enhancements: improved SHA1 performance, faster
    check and get on repositories with a long history and improved
    performance with very large files.

 -- David Roundy <droundy@abridgegame.org>  Thu,  1 Apr 2004 05:43:18 -0500

darcs (0.9.17)

  * fixed bug in darcs apply that made the --no-test option fail.
  * fixed bug that caused darcs to set file permissions to be
    non-world-readable.
  * darcs record and whatsnew can now accept file or directory arguments
    and limit their actions to changes in those files or directories.
  * darcs changes now can accept file or directory arguments and limit
    itself to changes affecting those files or directories.

 -- David Roundy <droundy@abridgegame.org>  Sat, 21 Feb 2004 08:12:34 -0500

darcs (0.9.16)

  * Add --sign-as=KEYID option to push command.
  * make optimize split up inventory for faster pulls
  * Allow use of a different make command for tests, such as gmake
  * Can now put prefs that would normally go in _darcs/prefs (defaults,
    binaries and boring) in ~/.darcs/ to set the prefs for all your
    repositories at once.
  * add primitive xml output to annotate of directory.
  * When pushing a patch, add the list of changes in the description.
  * refuse to rollback a patch twice, since that would cause problems.
  * make darcs diff accept optional arguments indicating files and
    directories to diff.
  * preserve permissions on files in working directory.
  * put docs in ...share/doc/darcs not share/darcs/doc.
  * add support for multiple-choice options.  This means that you can now
    set your default option in _darcs/prefs/defaults, and then override
    that default on the command line.
  * shortened --use-external-merge-tool option to --external-merge
  * more "boring" patterns.

 -- David Roundy <droundy@abridgegame.org>  Tue, 10 Feb 2004 07:08:14 -0500

darcs (0.9.15)

  * next step repository format transition--we use the new patch filenames.
  * fix handling of text files with no trailing newline--this will cause
    some trouble.  Darcs will require that you convert your repository
    using convert-repo.  This will leave you with a bunch of changes
    regarding trailing newlines which you will either want to record or
    revert.
  * the windows support is somewhat improved.
  * added simple "repair" command that can repair some kinds of
    inconsistencies in the repository.
  * added primitive "annotate" command to extract information about
    modifications to files and directories.
  * fixed handling of darcs mv to allow moving to directories in a more
    intuitive manner.
  * handling of binary files was dramatically improved in both memory and
    cpu usage.
  * added autoconf testing framework to clean up code dealing with
    different versions of ghc, features that don't exist on windows, bugs
    that only exist on windows, etc.
  * don't accept invalid flags.
  * add more patterns to boring and binary.
  * use autoconf test to handle posix signals and windows '\\' handling.
  * switch to using new patch filenames.
  * XML formatted output for 'changes' command
  * add support for unidiff-like whatsnew output.
  * fix bug in RTS memory calculation for large RAM size
  * add rollback command.
  * improve checkpointing support.
  * add diff-opts option to darcs diff.
  * add support for building docs using htlatex or hevea rather than
    latex2html.
  * use locking whereever it is needed.
  * add safe (atomic) writing of inventory files.

 -- David Roundy <droundy@abridgegame.org>  Fri, 12 Dec 2003 07:59:54 -0500

darcs (0.9.14)

  * darcs changes now shows times formatted according to current locale.
  * add support for automatically treating files containing ^Z or '\0' as
    binary.
  * add experimental checkpointing, allowing get to only download the
    recent change history.
  * allow darcs to be called within subdirectories of a repository.
  * make default be to compress patches.
  * add --summary option to whatsnew.
  * add trackdown command.
  * fix bug in darcs dist --verbose.
  * make darcs diff have closer behavior to cvs diff.  In particular, darcs
    diff with no arguments now gives you the difference between the working
    directory and the latest recorded version.
  * support external graphical merge tools.
  * fix bug where binary patch is created even with no change.
  * support darcs -v for version.  Also mention the darcs version in the
    usage mesage.
  * ignore empty lines in boring file and binary file.
  * preserve pending changes (e.g. file adds or darcs replaces) across
    revert and record.
  * create repositories with new patch filename format.
    The new repo format is now created alongside the old format, but the
    old format is read.  There is a tool called convert-repo that will
    convert an old format repo to have both formats.
  * use iso format for dates in record.
  * New patch-selecting interface.
    This patch only uses the new routine for revert, since it's not
    particularly well tested.  The text method now allows one to go back
    and edit previous patches.  The idea is that eventually all commands
    that need the user to select a subset of patches will use this routine.
  * use hash for cgi cache file names.
  * add preliminary experimental GUI support using wxhaskell.
  * remember author name after first record in a repo.
  * add unrevert command.
  * always match full pathnames when checking boringness or binaryness.
  * rewrite replace tokenizer for more speed.
  * make darcs compile with ghc 6.2 and later.
  * fix some bugs in darcs diff.
  * make --and-apply work locally as well as via ssh.
    Also added a --and-apply-as that uses sudo to run the apply command as
    a different user.

 -- David Roundy <droundy@abridgegame.org>  Mon, 10 Nov 2003 07:08:20 -0500

darcs (0.9.13)

  * Various performance enhancements.
  * add --pipe option to tag and record, which causes them to prompt for
    all their input, including date.  This can be useful when creating
    repository converters.
  * remove '-t' short command line option for '--to' and the '-o' short
    option for '--reponame'.
  * remove the darcs-patcher program.
        The functionality of the darcs-patcher program is taken over by
        the darcs apply command.  Several fancy features have been added,
        as described in the Apply section of the manual.
  * support spaces and (maybe) unicode in filenames.
  * updates to win32 support
  * push via ssh
  * add --without-libcurl option to configure
  * include DarcsURL in push email.
  * add support for reading and writing gzipped patch files.
  * allow multiple --to addresses on push, and also support --cc for
    additional addresses.
  * when pulling or pushing from lastrepo, say where lastrepo is.
  * only save lastrepo in get if the source repo wasn't a relative
    directory path.

 -- David Roundy <droundy@abridgegame.org>

darcs (0.9.12)

  * add manual section on building darcs.
  * improve scaling of checking for and resolving conflicts, which was an
    O(n^2) function.
  * escape ESC char when printing patches.
  * don't reorder patches unless necesary--this avoids an O(n^2) operation
    which was making a darcs record very slow when a lot of files were
    added.
  * fix default regexps for boring file (Thanks Trevor!)
  * replace now ignores files that aren't in the repo.
  * make darcs add refuse to add files whose subdirectories don't exist.
  * implement support for binary files.
  * added support for running external programs to fetch files.
  * fix conflict resolution bug from 0.9.11.
  * make the patcher run the test prior to applying.
  * add repo locking.
  * Fix bug when pulling from a repo containing just one patch (thanks
    Peter).
  * install cgi script in cgi-bin directory.

 -- David Roundy <droundy@abridgegame.org>

darcs (0.9.11)

  * A rewrite of the configure code and makefile (thanks to Peter Simons).
  * Added several new repository configuration options including a setpref
    command which allows you to set preferences options that are pulled
    from repo to repo.
  * Yet another rewrite of the merging code.
  * User can now revert changes on a change-by-change basis.
  * Yet another major improvement in speed and memory consumption.
  * Add a darcs diff command to compare two versions.

 -- David Roundy <droundy@abridgegame.org>  Mon, 30 Jun 2003 06:42:10 -0400

darcs (0.9.10)

  * Added a way to configure the default values for options to darcs
    commands.  See Appendix B of manual.
  * darcs push and pull now default to pulling and pushing from the most
    recently accessed repository (if you don't specify a repo).
  * Numerous bugfixes.

 -- David Roundy <droundy@abridgegame.org>  Wed, 21 May 2003 07:08:40 -0400

darcs (0.9.9)

  * Created a way to have a "centralized server".  (See darcs-patcher
    chapter in manual).
  * Added new darcs-server package.
  * Switch to new repository format.  Note that your repo will only be
    converted to the new format if you use certain commands such as
    unpull.  You can recognize the new format by the presence of a
    _darcs/inventories/ directory.
  * Add the ability to sign patches sent with push using gnupg and to
    verify those signatures when applying.  (This is the authentication
    basis for the above-mentioned server).
  * Fix bug in application of a file rename patch.

 -- David Roundy <droundy@abridgegame.org>  Thu,  8 May 2003 06:58:42 -0400

darcs (0.9.8)

  * Fix rare bug in check when files happen to be a multiple of 1024 bytes
    in length.
  * Fix bug in reading patch ids with long comments from local files.
  * Prepare for a change in the repository format.  The format doesn't
    change for this version, but version 0.9.8 is able to read the new
    repository format.

 -- David Roundy <droundy@abridgegame.org>  Wed, 30 Apr 2003 08:54:18 -0400

darcs (0.9.7)

  * Fix a couple of rename conflict bugs.
  * Add new test suite framework, along with several tests.
  * Several major optimizations for speed and memory.
  * Added --ignore-times option to not assume that when a file
    modification time hasn't changed the file itself hasn't changed.

 -- David Roundy <droundy@abridgegame.org>  Sat, 26 Apr 2003 07:57:01 -0400

darcs (0.9.6)

  * Fixed a couple of bugs in the merging of conflicting renames.
  * Added an interface to include long comments when recording.
  * Improve the interface of pull, allowing for viewing the patches before
    pulling them.
  * Include zsh command completion examples with docs.
  * Massively improved responsiveness in command completion.
  * Use packed strings to save memory.
  * Fixed a bug that shows up in empty repos.
  * Fixed multiple bugs in the mv command.

 -- David Roundy <droundy@abridgegame.org>  Thu, 17 Apr 2003 09:34:34 -0400

darcs (0.9.5)

  * Improve merge of creation of files and directories with the same name.
  * Add darcs push and apply commands, which are the beginning of work
    towards supporting a "centralized server" concept a la CVS.  However,
    they are also useful for a "Linus" style workflow, based on emailing
    patches.  In theory they could also be used to provide a smart server
    that could server pulls using less bandwidth.
  * Add an unpull command analagous to unrecord, but which removes the
    patches from the working directory also.
  * Enable the mv command, since the mv patches have now been supported by
    a couple of versions.
  * Include zsh_completion code, thanks to Aaron Denney <wnoise@ofb.net>.

 -- David Roundy <droundy@abridgegame.org>  Wed,  9 Apr 2003 07:52:01 -0400

darcs (0.9.4)

  * Speed up whatsnew and record in the case where there are huge numbers
    of extra files in the working directory.
  * Small (~10%) speedup in get.

 -- David Roundy <droundy@abridgegame.org>  Fri,  4 Apr 2003 09:08:38 -0500

darcs (0.9.3)

  * Optimized whatsnew and record by seting modification time of "current"
    files equal to that of working files if they are identical, so I won't
    have to check again if the working one hasn't been changed.
  * Rewrite file renaming code (no creation).
  * Add support for replacing tokens in files.
  * Make cgi output work more accurately, and point out which files were
    modified by each patch.
  * Add a caching feature to the cgi script to speed things up a bit.
  * Turn on creation of dependencies when recording.
  * Add a 'tag' command.
  * Rewrote the 'pull' code to hopefully speed it up (and in any case to
    greatly simplify it).

 -- David Roundy <droundy@abridgegame.org>  Thu,  3 Apr 2003 07:08:05 -0500

darcs (0.9.2)

  * Add build dependency on tetex and latex2html
  * Have internal diff code properly respond to deleted files and
    directories.
  * Create file and directory rename patch types. (no creation--which
    means that I am waiting to create commands to create such patches
    until later, to avoid backward compatibility issues of repos.)
  * Add support for patch dependencies. (no creation)
  * Add support for token replacement patches. (no creation)

 -- David Roundy <droundy@abridgegame.org>  Thu, 27 Mar 2003 07:59:09 -0500

darcs (0.9.1)

  * Make darcs get --verbose actually be verbose (which is important
    because it takes so long that the user might be afraid it's hanging.
  * Speed up the merge in complicated cases, possibly dramatically.
  * Add a darcs remove command.

 -- David Roundy <droundy@abridgegame.org>  Mon, 10 Mar 2003 09:48:55 -0500

darcs 0.9.0

  * Initial Release.

 -- David Roundy <droundy@abridgegame.org>  Wed,  3 Mar 2003 13:51:58 -0500



Local variables:
mode: outline
outline-regexp: "[dD]\\| +\\*+"
paragraph-separate: "[  ]*$"
end:
