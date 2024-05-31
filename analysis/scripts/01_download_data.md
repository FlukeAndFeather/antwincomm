The data for this analysis were deposited in the US Antarctic Program Data Center (USAP-DC). They can be found at <doi:10.15784/601795>. The USAP-DC does not permit automated downloads, so to reproduce this analysis you will need to manually download the data and put them in the correct directory. The dataset is contained in a zip file named AntPeninsula_winter_marine_data.zip. Extract the contents to analysis/data/raw_data. Your directory structure should look like this when complete:

```         
analysis/data/raw_data
├── AntPeninsula_winter_marine_data
│   ├── Ice.csv
│   ├── PredatorSightings.csv
│   ├── StationZooplanktonPhysics.csv
│   └── nor-ant-pen-winter-amlr.xml
└── AntPeninsula_winter_marine_data.zip
```

You can verify the files are in the correct location by running the following command in R. It should return `TRUE` 3 times (once for each data file).

```         
file.exists(
  file.path("analysis", "data", "raw_data", "AntPeninsula_winter_marine_data", 
            c("Ice.csv", "PredatorSightings.csv", "StationZooplanktonPhysics.csv"))
)
```
