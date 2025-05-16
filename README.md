# Create climate bulletin publications on the MeteoSwiss Website

Package documentation: https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/

## How to use 

### Produce the monthly climate bulletin

The CAT is used to produce the monthly climate bulletin for the [MeteoSwiss website](https://www.meteoschweiz.admin.ch/service-und-publikationen/publikationen.html#order=date-desc&page=1&pageGroup=publication&tenant=mchweb&category=climate).

See documentation of the  [bulletin_monthly](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/bulletin_monthly.html) function for how to produce it locally. 

Make sure to have the configuration setup necessary to access the product provider (see Configuration).

### Create your own publication

The creation of a publication is a two step process:

1) A bulletin object is assembled starting with [create_bulletin](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/create_bulletin.html) followed by adding elements by using functionality like

- [add_image](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/add_image.html)
- [add_Rmd](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/add_Rmd.html)
- [add_table](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/add_table.html)
- [add_text](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/add_text.html)
- [add_disclaimer](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/add_disclaimer.html)
- [add_link_list](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/add_link_list.html)
- [add_shorties](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/add_shorties_list.html)

See the function [create_test_bulletin_for_web](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/create_test_bulletin_for_web.html) to see how a basic bulletin is created.

2) The bulletin object is rendered to either pdf [bulletin_to_pdf](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/bulletin_to_pdf.html) or xml [bulletin_to_xml](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/bulletin_to_xml.html). xml format is required for publication as html on the MeteoSwiss website. 

The [bulletin_to_webzip](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/bulletin_to_webzip.html) function will create a zip file with both pdf and xml files that can be sent to the MeteoSwiss website.

The CMS elements that are supported by the website are documented on the Quatico Confluence:

[CMS elements for automatic publications](https://quatico.atlassian.net/wiki/spaces/MCHW2C/pages/414842883). A pdf of the documentation is available in the [documentation folder](inst/cms-doc/Schnittstellendefinition_Automatisierte_Publikationen.pdf)

### Auxiliary functions

There are some functions for modifying images:

- [crop_image](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/crop_image.html)
- [join_images](https://service.meteoswiss.ch/documentation/cat.bulletin/latest/R/reference/join_images.html)

# Configuration

## Download realizations from the product provider

The bulletin downloads images and data from the ProductBrowser ProductProvider https://service.meteoswiss.ch/pbproductprovider which requires authentication.

The authentication follows the MeteoSwiss Secure Inbound Traffic Architecture (see https://meteoswiss.atlassian.net/wiki/x/FAZ1)

Assuming a standard CATs installation, a valid offline token is expected in the environment variable `PRODUCT_PROVIDER_OLTOKEN`. See [CATs sample configuration files](https://meteoswiss.atlassian.net/wiki/spaces/APKTools/pages/1770411/Sample+configuration+files#Settings-for-gridget-and-mchdwh).

For information about obtaining authentication tokens, see [Using oauth2 authentication with mchdwh and gridget](https://meteoswiss.atlassian.net/wiki/x/6odcEw).

## Log output

### Info messages
use
`options(log_level = 1)`
to see only messages on info level.

### Debug messages
`options(log_level = 2)`
to see also debug information.


