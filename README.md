# ISO11784Tools

### Installation

`remotes::install_github("graemediack/ISO11784Tools",build_vignettes = TRUE)`

`vignette("ISO11784Tools")`


## Introduction
ISO11784 and ISO11785 are standards describing how information should be encoded within passive integrated transponder tags (PIT tags).

ISO11784 describes the binary construction of the information stored in the tag, and ISO11785 describes how it should be read by PIT tag readers.

Despite this, there are some differences in how readers will display the id information depending on user settings and manufacturer choices.

#### Further Reading

- [WIKIPEDIA](https://en.wikipedia.org/wiki/ISO_11784_and_ISO_11785)

- [RFID NEWS](https://www.rfidnews.com/ISOstandard/ISOstandard.html)

- [ISO ORG](https://www.iso.org/standard/25881.html)

- [PRIORITY DESIGN](https://www.priority1design.com.au/fdx-b_animal_identification_protocol.html)

### ISO11784 In Practice

I encountered 4 different formats of ISO11784 ID codes whilst researching for a PIT tag database. This package is intended to facilitate transformation to and from all of these formats.

It could potentially have other functionality with respect to ISO11784, for example it could connect to ICAR and identify manufacturer of tags along with other metadata.

The 4 formats identified are:

- `Dot Hexadecimal` - e.g. **3E7.1CBE991A14**. Manufacturer Code = **3E7** (Note - this is ID is reserved for testing), Animal ID = **1CBE991A14**
- `Full Decimal` - e.g. **999123456789012**. Manufacturer Code = **999** (Note - this is ID is reserved for testing), Animal ID = **123456789012**
- `Full 64 bit Hexadecimal` in two 'flavours' that depend on whether the device reverses the transmitted binary ID before converting it to hex or not:
  - `Left Hand Flavour` - e.g. **8000F9DCBE991A14**. 
  - `Right Hand Flavour` - e.g. **2858997D3B9F0001**.

Note, all 4 examples above are the same code.

## Functions

I have created a set of functions to make converting between these formats easy.

### get_iso11784_format

pass a vector of strings containing your tag codes into this function to obtain a vector of the formats
```{r, include=T,warning = FALSE}
ISO11784Tools::get_iso11784_format(c('3E7.1CBE991A14','999123456789012','8000F9DCBE991A14','2858997D3B9F0001','blahblah'))
```

### convert_to_X Family

The convert_to_X family of functions takes a vector of strings with your tag codes and converts them into the target format
```{r, include=T,warning=F}
ISO11784Tools::convert_to_isodecimal(c('3E7.1CBE991A14','999123456789012','8000F9DCBE991A14','2858997D3B9F0001','blahblah'))
ISO11784Tools::convert_to_isodothex(c('3E7.1CBE991A14','999123456789012','8000F9DCBE991A14','2858997D3B9F0001','blahblah'))
ISO11784Tools::convert_to_iso64bitl(c('3E7.1CBE991A14','999123456789012','8000F9DCBE991A14','2858997D3B9F0001','blahblah'))
ISO11784Tools::convert_to_iso64bitr(c('3E7.1CBE991A14','999123456789012','8000F9DCBE991A14','2858997D3B9F0001','blahblah'))
```

### convert_to_all
```{r, include=T,warning=F}
ISO11784Tools::convert_to_all(c('3E7.1CBE991A14','999123456789012','8000F9DCBE991A14','2858997D3B9F0001','blahblah'))
```

`convert_to_all` takes a vector of strings and returns a 6 column tibble with detected format and all the possible conversions. It simply wraps the previous 5 functions into one and outputs as a tibble to save some time for the user

### _to_ Family

The `convert_to_X` family of functions are wrappers for lower level functions that convert to and from the `Full Decimal` format. The `Full Decimal` format and the `Dot Hexadecimal` format were the first I encountered, and seem to be the most prevalent. I chose `Full Decimal` as the primary format for this package, and created functions to convert from and to that format in the early stages of development. Hence, these functions center on `Full Decimal`.

Rather than give full examples of these I will simply list them here for reference. Each of these functions will accept a single string or a vector of strings and will return `warning` and `NA` if the format is not as expected, but will still convert anything within a vector that is the correct format.

- From `Full Decimal`:

  - `isodecimal_to_iso64bitleft`
  - `isodecimal_to_iso64bitright`
  - `isodecimal_to_isodothex`
  
- To `Full Decimal`:

  - `iso64bitleft_to_isodecimal`
  - `iso64bitright_to_isodecimal`
  - `isodothex_to_isodecimal`
  
### Lowest Level Family

The above functions are built on base converters that are included as imported functions for convenience rather than intended for direct use.

These functions are written to allow conversion between the three bases; binary, decimal, and hexadecimal, to the maximum limit of the R `numeric` data type; `0b1000000000000000000000000000000000000000000000000000000`, `18014398509481984`, and `0x40000000000000` respectively. 

`R` has limitations with regard to maximum values held in `integer` and `numeric` data types. I don't know why this is so I won't try to expand on the reason here. ISO 11784 ID codes are generally larger than the `integer` maximum, but smaller than the `numeric` maximum. Other packages might have quicker versions of these functions that I haven't found (happy to receive suggestions!), so I needed to build them from scratch.

- `binary_to_decimal`
- `binary_to_hexadecimal`
- `decimal_to_binary`
- `decimal_to_hexadecimal`
- `hexadecimal_to_binary`
- `hexadecimal_to_decimal`

### The End. Enjoy!
