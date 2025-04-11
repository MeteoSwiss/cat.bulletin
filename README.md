# Create climate bulletin publications on the MeteoSwiss Website

Package documentation: https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/

# Configuration

## Download realizations from the product provider

The bulletin downloads images and data from the ProductBrowser ProductProvider https://service.meteoswiss.ch/pbproductprovider which requires authentication.

The authentication follows the MeteoSwiss Secure Inbound Traffic Architecture (see https://meteoswiss.atlassian.net/wiki/x/FAZ1)

Assuming a standard CATs installation, a valid offline token is expected in the environment variable `PRODUCT_PROVIDER_OLTOKEN`.

See https://meteoswiss.atlassian.net/wiki/x/6odcEw for configuration.

## Log output

### Info messages
use
`options(log_level = 1)`
to see only messages on info level

### Debug messages
`options(log_level = 2)`
to see also debug information

##  Where to start

See the function `create_test_bulletin_for_web(workdir = ".")` to see how a basic bulletin is created.

