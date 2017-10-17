# http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/

library(acs)

api.key.install("ab664ab627f56ed01df0b97a25f6f473598a7fec")

# no urban/rural update available
acs.fetch(dataset = "sf1",
          endyear = 2010,
          geography = geo.make(state = "RI", county = "*"),
          variable = c("P0020001", "P0020002", "P0020003", "P0020004", "P0020005"))

    #                   P0020001     P0020002 P0020003 P0020004 P0020005
    # Bristol County    49875 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # Kent County       166158 +/- 0 0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # Newport County    82888 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # Providence County 626667 +/- 0 0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # Washington County 126979 +/- 0 0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0

# cannot get block data
# the lowest is block group
acs.fetch(dataset = "sf1",
          endyear = 2010,
          geography = geo.make(state = "RI", county = "Kent", tract = "*", block = "*"),
          variable = c("P0020001", "P0020002", "P0020003", "P0020004", "P0020005"))

    #               P0020001   P0020002 P0020003 P0020004 P0020005
    # Block Group 1 2505 +/- 0 0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # Block Group 2 1355 +/- 0 0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # Block Group 3 2578 +/- 0 0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # Block Group 1 1563 +/- 0 0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # Block Group 2 1304 +/- 0 0 +/- 0  0 +/- 0  0 +/- 0  0 +/- 0
    # ............................................................
