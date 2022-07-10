# ISO11784Tools
R package for working with codes read from ISO11784 encoded PIT tags

## ISO 11784
ISO 11784 and ISO 11785 are standards describing how information should be encoded within passive integrated transponder tags (PIT tags).

The standards describes the binary construction of the information stored in the tag, and how it should be read by PIT tag readers

Despite this, there are some differences in how readers will display the id information depending on settings and manufacturer choices.

I encountered 4 different formats of ISO11784 ID codes whilst researching for a PIT tag database. This package is intended to facilitate transformation to and from all of these formats.

It could potentially have other functionality with respect to ISO11784, for example it could connect to ICAR and identify manufacturer of tags along with other metadata
