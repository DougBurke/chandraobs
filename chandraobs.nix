{ mkDerivation, aeson, base, blaze-builder, blaze-html
, blaze-markup, bytestring, Cabal, conduit, containers
, data-default, formatting, groundhog, groundhog-postgresql
, groundhog-th, heroku, http-conduit, http-types, monad-control
, parsec, process, random, resource-pool, scotty, split, stdenv
, tagsoup, temporary, text, time, transformers, vector
, vector-algorithms, wai-middleware-static, warp, zlib
}:
mkDerivation {
  pname = "chandraobs";
  version = "0.3.0";
  src = ./.;
  configureFlags = [ "-fserver" "-ftools" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base blaze-html bytestring Cabal containers formatting
    groundhog groundhog-postgresql groundhog-th heroku http-types
    monad-control scotty split text time transformers vector
    vector-algorithms zlib
  ];
  executableHaskellDepends = [
    aeson base blaze-builder blaze-html blaze-markup bytestring conduit
    containers data-default formatting groundhog groundhog-postgresql
    http-conduit http-types parsec process random resource-pool scotty
    split tagsoup temporary text time transformers
    wai-middleware-static warp
  ];
  description = "What is the Chandra Observatory doing?";
  license = stdenv.lib.licenses.publicDomain;
}
