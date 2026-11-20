# Gale-Shapley Master Track Placement

Places students in master tracks with a college-admissions (Gale-Shapley, deferred acceptance) algorithm with track quotas. Ties within a qualification level are broken by a seeded lottery. Raoul Grasman, University of Amsterdam.

The repository holds two versions of the same app.

## `shiny/`: original R Shiny app (2018)

- `app/app.R`, `app/GaleShapely.R`: the original app, unchanged.
- `packrat.lock`: the package versions of the 2018 deployment (R 3.4.4).
- `Dockerfile`, `install_lock.R`, `reinstall_ordered.R`, `shiny-server.conf`: a Docker image with R 3.4.4, Shiny Server 1.5.9 and exactly the locked package versions.
  - Packages are installed twice. The second pass goes in dependency order, because a package built while a newer version of one of its dependencies is installed keeps stale code.
  - The app's `val.Rdata` is linked to `/dev/null`, so no student data is stored on disk.
  - The R process stops 30 minutes after the last visitor (`app_idle_timeout`).
  - At most 3 simultaneous sessions (`simple_scheduler`).
- `deploy/`: the nginx location and rate limits, and the hourly cleanup job used on the server.

Build and run:

```sh
cd shiny
docker build -t mastertrack:r3.4.4 .
docker run -d --name mastertrack --restart unless-stopped \
  -p 127.0.0.1:3838:3838 --memory 1g --cpus 1 mastertrack:r3.4.4
# app at http://127.0.0.1:3838/mastertrack/
```

## `docs/`: static HTML + JavaScript version

`index.html` plus `app.js`; no build step and no server needed. Open `index.html` in a browser, or publish the folder with GitHub Pages (Settings → Pages → branch `main`, folder `/docs`). The libraries load from a CDN: SheetJS 0.18.5 for Excel files and Chart.js 4.4.1.

- **R's random numbers:** reproduced exactly. That covers the Mersenne-Twister seeding of `set.seed`, and both `sample()` methods: "Rejection" (R ≥ 3.6) and "Rounding" (R < 3.6, as in the 2018 deployment).
- **Default seed:** today's date (YYYYMMDD).
- **Computation code:** has no DOM code and also loads in Node (`require('./app.js')`).

### Example data

`docs/examples/` has synthetic test data (generated names, `@example.org` addresses). Each folder holds an applications file and a quota file to upload together:

- `small-30-students`: 30 students, 4 tracks, extra Name/Email columns.
- `no-preference-column`: no Preference column (row order is the preference) and no extra columns.
- `many-unqualified`: about half of the applications are "O" (unqualified).
- `all-equal-qualification-lottery`: everyone has qualification "E", so the lottery decides.
- `large-400-students`: 400 students, 8 tracks, mixed-case and punctuated names.

### Known difference from the 2018 deployment

The JS version was checked against R 4.3 / dplyr 1.1 and gives identical output there. dplyr 0.7 (2018) sorts text with the system locale, whereas dplyr ≥ 1.1 `arrange()` uses C (byte) order. Against R 3.4.4 / dplyr 0.7, this changes:
- the row order of the downloaded CSV;
- with mixed-case or punctuated names, which student gets which lottery number.

## `tests/`

- `reference.R` runs the app's server logic without Shiny and writes all intermediate and final tables.
- `compare.js` runs `docs/app.js` in Node on the same inputs and compares the results exactly.
- `gen.R` generates the test cases.
- `browser_test.js` drives the page in a headless browser.

In a config, point `"galeshapely"` to `../shiny/app/GaleShapely.R`.

Issues found in the original code:
- The waiting list fails without a quota file.
- The waiting list fails when the withdrawal percentage rounds down to 0 students.
- The download fails if the track column is not named "Track".

The JS version handles these cases instead of crashing.
