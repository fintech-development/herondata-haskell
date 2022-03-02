#! /bin/bash

curl https://app.herondata.io/swagger/ -o swagger.json

# https://openapi-generator.tech/docs/installation
openapi-generator-cli generate -i swagger.json -g haskell-http-client -o src/ --skip-validate-spec
