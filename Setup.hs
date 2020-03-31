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
          hs_builddir <- makeAbsolute $ buildDir lbi
          let verbosity = fromFlag (configVerbosity (configFlags lbi))
              cxx_builddir = hs_builddir </> "binaryen"
              run prog args =
                let Just conf_prog =
                      lookupProgram (simpleProgram prog) (withPrograms lbi)
                 in runProgramInvocation verbosity (programInvocation conf_prog args)
          createDirectoryIfMissing True cxx_builddir
          cwd <- getCurrentDirectory
          withCurrentDirectory cxx_builddir
            $ for_
              [ [ "-DCMAKE_BUILD_TYPE=Release",
                  "-DBUILD_STATIC_LIB=ON",
                  "-G",
                  "Unix Makefiles",
                  cwd </> "binaryen"
                ],
                ["--build", cxx_builddir]
              ]
            $ \args -> run "cmake" args
          lib_hs_list <-
            map (hs_builddir </>)
              . filter ("libHSbinaryen" `isPrefixOf`)
              <$> listDirectory hs_builddir
          for_ lib_hs_list $ \lib_hs -> do
            ar_hs <- afilter isObj <$> loadAr lib_hs
            ar_cxx <-
              afilter isObj
                <$> loadAr (cxx_builddir </> "lib" </> "libbinaryen.a")
            writeGNUAr lib_hs $ ar_hs <> ar_cxx
            run "ranlib" ["-D", lib_hs],
        copyHook = \pkg_descr lbi hooks flags -> do
          copyHook simpleUserHooks pkg_descr lbi hooks flags
          hs_builddir <- makeAbsolute $ buildDir lbi
          let cxx_builddir = hs_builddir </> "binaryen"
              [clbi] = componentNameCLBIs lbi mainLibName
              binaryen_installdirs =
                absoluteComponentInstallDirs
                  pkg_descr
                  lbi
                  (componentUnitId clbi)
                  (fromFlag (copyDest flags))
              binaryen_bindir = bindir binaryen_installdirs
          createDirectoryIfMissing True binaryen_bindir
          binaryen_bins <- listDirectory $ cxx_builddir </> "bin"
          for_ binaryen_bins $ \b ->
            renameFile (cxx_builddir </> "bin" </> b) (binaryen_bindir </> b)
      }

isObj :: ArchiveEntry -> Bool
isObj = (`elem` [".o", ".p_o"]) . takeExtension . filename

mainLibName :: ComponentName
#if MIN_VERSION_Cabal(3,0,0)
mainLibName = CLibName defaultLibName
#else
mainLibName = defaultLibName
#endif
