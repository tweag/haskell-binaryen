{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -O -threaded -rtsopts "-with-rtsopts=-A64m -n2m -I0 -qg" #-}

import Ar
import Data.Foldable
import Data.List
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Types.LocalBuildInfo
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = map simpleProgram ["cmake", "ranlib"],
        buildHook = \pkg_descr lbi hooks flags -> do
          buildHook simpleUserHooks pkg_descr lbi hooks flags
          let verbosity = fromFlag (configVerbosity (configFlags lbi))
              hs_builddir = buildDir lbi
              cxx_builddir = binaryenBuildDir lbi
              run prog args =
                let Just conf_prog =
                      lookupProgram (simpleProgram prog) (withPrograms lbi)
                 in runProgramInvocation verbosity (programInvocation conf_prog args)
          for_
            [ [ "-DCMAKE_BUILD_TYPE=Release",
                "-DBUILD_STATIC_LIB=ON",
                "-G",
                "Unix Makefiles",
                "-S",
                "binaryen",
                "-B",
                cxx_builddir
              ],
              ["--build", cxx_builddir]
            ]
            $ \args -> run "cmake" args
          Just lib_hs <-
            fmap (hs_builddir </>)
              . find ("libHSbinaryen" `isPrefixOf`)
              <$> listDirectory hs_builddir
          ar_hs <- afilter isObj <$> loadAr lib_hs
          ar_cxx <-
            afilter isObj
              <$> loadAr (cxx_builddir </> "lib" </> "libbinaryen.a")
          writeGNUAr lib_hs $ ar_hs <> ar_cxx
          run "ranlib" ["-D", lib_hs],
        copyHook = \pkg_descr lbi hooks flags -> do
          copyHook simpleUserHooks pkg_descr lbi hooks flags
          let binaryen_builddir = binaryenBuildDir lbi
              [clbi] = componentNameCLBIs lbi mainLibName
              binaryen_installdirs =
                absoluteComponentInstallDirs
                  pkg_descr
                  lbi
                  (componentUnitId clbi)
                  (fromFlag (copyDest flags))
              binaryen_bindir = bindir binaryen_installdirs
          createDirectoryIfMissing True binaryen_bindir
          binaryen_bins <- listDirectory $ binaryen_builddir </> "bin"
          for_ binaryen_bins $ \b ->
            renameFile (binaryen_builddir </> "bin" </> b) (binaryen_bindir </> b)
      }

isObj :: ArchiveEntry -> Bool
isObj = (== ".o") . takeExtension . filename

binaryenBuildDir :: LocalBuildInfo -> FilePath
binaryenBuildDir = (</> "binaryen") . buildDir

mainLibName :: ComponentName
#if MIN_VERSION_Cabal(3,0,0)
mainLibName = CLibName defaultLibName
#else
mainLibName = defaultLibName
#endif
