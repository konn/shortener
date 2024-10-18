VERSION 0.8
ARG --global GHC_VER=9.10.1
ARG --global GLOBAL_CACHE_IMAGE=ghcr.io/konn/shortener/build-cache
FROM --platform=linux/amd64 ghcr.io/konn/ghc-wasm-earthly:${GHC_VER}
WORKDIR /workdir

ENV GHC=wasm32-wasi-ghc
ENV CABAL=wasm32-wasi-cabal --project-file=cabal-wasm.project \
--with-compiler=wasm32-wasi-ghc-${GHC_VER} \
--with-ghc=wasm32-wasi-ghc-${GHC_VER} \
--with-ghc-pkg=wasm32-wasi-ghc-pkg-${GHC_VER} \
--with-hc-pkg=wasm32-wasi-ghc-pkg-${GHC_VER} \
--with-hsc2hs=wasm32-wasi-hsc2hs-${GHC_VER}

ENV MOUNT_GLOBAL_STORE="type=cache,mode=0777,id=all#ghc-${GHC_VER}#global-store,sharing=shared,target=/root/.ghc-wasm/.cabal/store"
ENV MOUNT_DIST_NEWSTYLE="type=cache,mode=0777,id=all#ghc${GHC_VER}#dist-newstyle,sharing=shared,target=dist-newstyle"

build-all:
  COPY --keep-ts . .
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} update --index-state=2024-10-17T07:25:36Z
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} build --only-dependencies all
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} build all

build:
  FROM +build-all
  BUILD +build-all
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  # From frontend/build.sh in tweag/ghc-wasm-miso-examples
  LET HS_WASM_PATH=$(${CABAL} list-bin -v0 ${target})
  LET WASM_LIB=$(wasm32-wasi-ghc --print-libdir)
  LET DEST=dist/${wasm}
  RUN mkdir -p dist
  RUN --mount ${MOUNT_DIST_NEWSTYLE} cp ${HS_WASM_PATH} ./dist/${wasm}
  RUN --mount ${MOUNT_DIST_NEWSTYLE} ${WASM_LIB}/post-link.mjs --input ${HS_WASM_PATH} --output ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT dist

optimised-wasm:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  RUN mkdir -p dist/
  BUILD +build --target=${target} --outdir=${outdir} --wasm=${wasm}.orig
  COPY (+build/dist/${wasm}.orig --target=${target} --outdir=${outdir} --wasm=${wasm}.orig) ./dist/
  RUN wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/${wasm} dist/${wasm}.orig
  RUN wasm-opt -Oz dist/${wasm} -o dist/${wasm}
  RUN wasm-tools strip -o dist/${wasm} dist/${wasm}
  COPY (+build/dist/ghc_wasm_jsffi.js --target=${target} --outdir=${outdir} --wasm=${wasm}.orig) ./dist/
  RUN rm ./dist/${wasm}.orig
  SAVE ARTIFACT dist

patch-jsffi-for-cf:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  BUILD +optimised-wasm --target=${target} --outdir=${outdir} --wasm=${wasm}
  COPY  (+optimised-wasm/dist --target=${target} --outdir=${outdir} --wasm=${wasm}) ./dist
  LET PATCHER=./js-ffi-patcher.mjs
  COPY ./build-scripts/jsffi-patcher.mjs ${PATCHER}
  RUN node ${PATCHER} ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT ./dist

frontend:
  BUILD +optimised-wasm --target=shortener-frontend:exe:shortener-frontend
  COPY (+optimised-wasm/dist --target=shortener-frontend:exe:shortener-frontend) ./dist
  LET ORIG_WASM=shortener-frontend.wasm
  LET SHASUM_WASM=$(sha1sum dist/${ORIG_WASM} | cut -c1-7)
  LET FINAL_WASM=shortener-frontend-${SHASUM_WASM}.wasm
  RUN mv dist/${ORIG_WASM} dist/${FINAL_WASM}

  LET GHC_JSFFI_ORIG=ghc_wasm_jsffi.js
  LET SHASUM_JSFFI=$(sha1sum dist/${GHC_JSFFI_ORIG} | cut -c1-7)
  LET GHC_JSFFI_FINAL=ghc_wasm_jsffi-${SHASUM_JSFFI}.js
  RUN mv dist/${GHC_JSFFI_ORIG} dist/${GHC_JSFFI_FINAL}

  COPY shortener-frontend/data/index.js dist/index.js
  RUN sed -i "s/${ORIG_WASM}/${FINAL_WASM}/g" dist/index.js
  RUN sed -i "s/${GHC_JSFFI_ORIG}/${GHC_JSFFI_FINAL}/g" dist/index.js
  LET INDEX_JS_SHASUM=$(sha1sum dist/index.js | cut -c1-7)
  LET INDEX_JS_FINAL=index-${INDEX_JS_SHASUM}.js
  RUN mv dist/index.js dist/${INDEX_JS_FINAL}
  COPY shortener-frontend/data/index.html dist/index.html
  RUN sed -i "s/index.js/${INDEX_JS_FINAL}/g" dist/index.html

  SAVE ARTIFACT ./dist AS LOCAL _build/frontend

worker:
  COPY shortener-worker/data/worker-template/ ./dist/
  BUILD +patch-jsffi-for-cf --target=shortener-worker:exe:shortener-worker --wasm=worker.wasm
  COPY (+patch-jsffi-for-cf/dist --target=shortener-worker:exe:shortener-worker --wasm=worker.wasm) ./dist/src
  RUN cd ./dist && npm i
  BUILD +frontend
  COPY +frontend/dist/* ./dist/assets/admin/
  SAVE ARTIFACT ./dist AS LOCAL _build/worker
  SAVE IMAGE
