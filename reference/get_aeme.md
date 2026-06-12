# Get Aeme object from LimnoTrack API

This function retrieves an Aeme object from the LimnoTrack API using the
provided lake ID. The Aeme object is returned as an R object.

## Usage

``` r
get_aeme(id = "LID1", api_url = "https://api.limnotrack.com", api_key = NULL)
```

## Arguments

- id:

  character or numeric; the character Lernzmp ID (e.g. "LID 1") of the
  lake or the numeric FENZ ID (e.g. 1).

- api_url:

  character; base URL of the API.

- api_key:

  character; API key for authentication. If NULL, will look for the key
  in the LERNZMP_KEY environment variable. If that is not set, will
  throw an error.

## Value

An Aeme object.

## Examples

``` r
aeme <- get_aeme(id = "LID1", api_key = Sys.getenv("LERNZMP_KEY"))
aeme
#> 
#> ── AEME ────────────────────────────────────────────────────────────────────────
#> 
#> ── Lake ──
#> 
#> Onoke (ID: LID1)
#> • Lat: -41.38; Lon: 175.13
#> • Elev: 0m; Depth: 8.72m; Area: 6534740 m2
#> 
#> ── Time ──
#> 
#> • Start: 2013-07-01; Stop: 2023-06-30; Time step: 3600
#> • Spin up (days): GLM: 1095; GOTM: 1095; DYRESM: 1095
#> 
#> ── Configuration ──
#> 
#> • Model: glm_aed and gotm_wet
#> • Path: Not set
#> • Model controls: Present
#> • Use biogeochemical model:
#> ┌ Model Configuration ─────────────────────────────────────────┐
#> │       Model              Physical         Biogeochemical     │
#> │ ---                                                          │
#> │       DY-CD               Absent              Absent         │
#> │      GLM-AED             Present              Absent         │
#> │      GOTM-WET            Present              Absent         │
#> └──────────────────────────────────────────────────────────────┘
#> 
#> ── Observations ──
#> 
#> • Lake: Present; Level: Absent
#> 
#> ── Input ──
#> 
#> • Initial profile: Present; Initial depth: 8.72m
#> • Hypsograph: Present (n=44)
#> • Meteo: Present; Use longwave: TRUE; Kw: 3.73626373626374
#> 
#> ── Inflows ──
#> 
#> • Number of inflows: 3; Names: NZS9266330, lumped, precip
#> • Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1
#> 
#> ── Outflows ──
#> 
#> • Number of outflows: 1; Names: wbal; Elevations:
#> • Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1
#> 
#> ── Water Balance ──
#> 
#> • Method: 2; Use: obs
#> • Modelled: Absent; Water balance: Present
#> 
#> ── Parameters ──
#> 
#> • Number of parameters: 0
#> 
#> ── Output ──
#> 
#> • DY-CD: 1
#> • GLM-AED: 1
#> • GOTM-WET: 1
#> • Variables: 63
#> Water temperature, Thermocline depth, Dissolved oxygen, Total chlorophyll a,
#> Total nitrogen, Total phosphorus, Water level, Volume, Change in volume,
#> Surface area, ... and 53 more
```
