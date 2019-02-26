{ mkDerivation, aeson, base, blaze-builder, blaze-html
, blaze-markup, bytestring, conduit, containers, data-default
, formatting, groundhog, groundhog-postgresql, groundhog-th, heroku
, http-conduit, http-types, monad-control, network, parsec, process
, random, resource-pool, scotty, split, stdenv, tagsoup, temporary
, text, time, transformers, wai-middleware-static, warp, zlib
}:
mkDerivation {
  pname = "chandraobs";
  version = "0.2.0";
  src = ./.;
  configureFlags = [ "-fserver" "-ftools" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base blaze-html bytestring containers formatting groundhog
    groundhog-postgresql groundhog-th heroku http-types monad-control
    scotty split text time transformers zlib
  ];
  executableHaskellDepends = [
    aeson base blaze-builder blaze-html blaze-markup bytestring conduit
    containers data-default formatting groundhog groundhog-postgresql
    http-conduit http-types network parsec process random resource-pool
    scotty split tagsoup temporary text time transformers
    wai-middleware-static warp
  ];
  description = "What is the Chandra Observatory doing?";
  license = stdenv.lib.licenses.publicDomain;
}