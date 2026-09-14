{
  mkDerivation,
  lib,
  src,
  makeWrapper,
  git,
  base,
  aeson,
  async,
  base64-bytestring,
  bytestring,
  case-insensitive,
  containers,
  crypton,
  directory,
  filelock,
  filepath,
  http-types,
  memory,
  myque,
  network,
  process,
  sqlite-simple,
  stm,
  temporary,
  text,
  time,
  unix,
  wai,
  warp,
  hspec,
}:
mkDerivation {
  pname = "chilin";
  version = "0.1.0.0";
  inherit src;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    aeson
    async
    base64-bytestring
    bytestring
    case-insensitive
    containers
    crypton
    directory
    filelock
    filepath
    http-types
    memory
    myque
    network
    process
    sqlite-simple
    stm
    temporary
    text
    time
    unix
    wai
    warp
  ];
  executableHaskellDepends = [
    base
    directory
    http-types
    text
  ];
  testHaskellDepends = [
    base
    aeson
    bytestring
    containers
    directory
    filepath
    hspec
    myque
    temporary
    text
  ];
  testToolDepends = [ git ];
  buildTools = [ makeWrapper ];
  postInstall = ''
    wrapProgram $out/bin/chilin --set CHILIN_GIT ${git}/bin/git
  '';
  description = "Git hosting with myque-backed collaboration";
  license = lib.licenses.bsd3;
  mainProgram = "chilin";
}
