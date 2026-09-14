{
  lib,
  stdenv,
  nodejs,
  pnpm,
  pnpmConfigHook,
  fetchPnpmDeps,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "chilin-web";
  version = "0.1.0.0";

  src = lib.cleanSourceWith {
    src = ./.;
    filter =
      path: _:
      !(builtins.elem (baseNameOf path) [
        "node_modules"
        "dist"
      ]);
  };

  nativeBuildInputs = [
    nodejs
    pnpm
    pnpmConfigHook
  ];

  # Regenerate after changing pnpm-lock.yaml: set the hash to lib.fakeHash,
  # run `nix build .#web`, and copy the value the mismatch reports.
  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 4;
    hash = "sha256-YzOKAzKqzbzyRkGULkanVEGSRThLZr8vYA8HuYxii8I=";
  };

  buildPhase = ''
    runHook preBuild
    pnpm run build
    runHook postBuild
  '';

  # A static bundle: the reverse proxy serves this directory and forwards
  # /api, /health and the Git routes to chilin on the same origin.
  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r dist/* $out/
    runHook postInstall
  '';
})
