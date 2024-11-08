-- |This module should only be imported by Darcs.UI.Options.*
-- and by 'Darcs.UI.Flags'. Other modules needing access to 'DarcsFlag'
-- should import 'Darcs.UI.Flags'
module Darcs.UI.Options.Flags ( DarcsFlag(..) ) where

import Darcs.Prelude

import Darcs.Util.Path ( AbsolutePath, AbsolutePathOrStd )

-- | The 'DarcsFlag' type is a list of all flags that can ever be
-- passed to darcs, or to one of its commands.
data DarcsFlag = Version | ExactVersion | ListCommands
               | Help | ListOptions | NoTest | Test
               | OnlyChangesToFiles | ChangesToAllFiles
               | LeaveTestDir | NoLeaveTestDir
               | Timings | Debug | DebugHTTP
               | Verbose | NormalVerbosity | Quiet
               | To String | Cc String
               | Output AbsolutePathOrStd | OutputAutoName AbsolutePath | Mail
               | Subject String | InReplyTo String | Charset String
               | SendmailCmd String | Author String | SelectAuthor | PatchName String
               | OnePatch String | SeveralPatch String
               | AfterPatch String | UpToPatch String
               | OnePattern String | SeveralPattern String
               | AfterPattern String | UpToPattern String
               | OneHash String | AfterHash String | UpToHash String
               | OneTag String | SeveralTag String | AfterTag String | UpToTag String
               | LastN String | MaxCount String
               | IndexRange String | OneIndex String
               | NumberPatches
               | GenContext | Context AbsolutePath | Count
               | LogFile AbsolutePath | RmLogFile | DontRmLogFile
               | DistName String | DistZip | All
               | Recursive | NoRecursive
               | Minimize | NoMinimize
               | Reorder | NoReorder
               | RestrictPaths | DontRestrictPaths
               | AskDeps | NoAskDeps | IgnoreTimes | DontIgnoreTimes
               | LookForAdds | NoLookForAdds
               | LookForMoves | NoLookForMoves
               | LookForReplaces | NoLookForReplaces
               | UseMyersDiff | UsePatienceDiff
               | Intersection | Union | Complement
               | Sign | SignAs String | NoSign | SignSSL String
               | HappyForwarding | NoHappyForwarding
               | Verify AbsolutePath | VerifySSL AbsolutePath
               | RemoteDarcsOpt String
               | EditDescription | NoEditDescription
               | Canonize | NoCanonize
               | Toks String
               | EditLongComment | NoEditLongComment | PromptLongComment
               | KeepDate | NoKeepDate
               | AllowConflicts | MarkConflicts | NoAllowConflicts
               | SkipConflicts
               | Boring | SkipBoring
               | AllowCaseOnly | DontAllowCaseOnly
               | AllowWindowsReserved | DontAllowWindowsReserved
               | DontGrabDeps | DontPromptForDependencies | PromptForDependencies
               | Compress | NoCompress | UnCompress
               | WorkRepoDir String | WorkRepoUrl String
               | NewRepo String
               | NotInRemote (Maybe String)
               | Reply String | ApplyAs String
               | MachineReadable | HumanReadable
               | Pipe | Interactive
               | DiffCmd String
               | ExternalMerge String | Summary | NoSummary
               | PauseForGui | NoPauseForGui
               | Unified | NonUnified | Reverse | Forward
               | Complete | Lazy
               | DiffFlags String
               | XMLOutput
               | ForceReplace
               | NonApply | NonVerify | NonForce
               | DryRun
               | InheritDefault | NoInheritDefault
               | SetDefault | NoSetDefault
               | Disable | SetScriptsExecutable | DontSetScriptsExecutable
               | Once | Linear | Backoff | Bisect
               | ShrinkFailure | NoShrinkFailure
               | Hashed -- deprecated flag, here to output an error message
               | UseFormat1 | UseFormat2 | UseFormat3
               | UseNoWorkingDir | UseWorkingDir
               | Sibling AbsolutePath
               | Files | NoFiles | Directories | NoDirectories
               | Pending | NoPending
               | PosthookCmd String | NoPosthook | AskPosthook | RunPosthook
               | PrehookCmd String  | NoPrehook  | AskPrehook  | RunPrehook
               | UMask String
               | StoreInMemory | ApplyOnDisk
               | NoHTTPPipelining
               | Packs | NoPacks
               | NoCache
               | AllowUnrelatedRepos
               | Check | Repair | JustThisRepo
               | ReadMarks AbsolutePath | WriteMarks AbsolutePath
               | NullFlag
               | NoAmendUnrecord | AmendUnrecord
               | PatchIndexFlag
               | NoPatchIndexFlag
               | EnumPatches | NoEnumPatches
               | WithPrefsTemplates | NoPrefsTemplates
               | OptimizeDeep | OptimizeShallow
                 deriving ( Eq, Show )
