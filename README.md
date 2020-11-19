#Cron Expression Parser (Deliveroo)

Original task description could be found in `Cron_Parser_Task.pdf`

Application was developed using JDK 15 so update your system accordingly if needed.

##Running the application

Application is packaged to a fat JAR by `sbt-assembly` plugin via `sbt assembly` call. So the JRE installed is a prerequisite to run the app.

Typical application call could look like this:

```
java -jar cronparse.jar "*/15 0 1,15 * 1-5 2021-2025 /usr/bin/find
```

Output is provided in format like

```
minute        0 15 30 45
hour          0
day of month  1 15
month         1 2 3 4 5 6 7 8 9 10 11 12
day of week   1 2 3 4 5
year          2021 2022 2023 2024 2025
command       /usr/bin/find

```

##Running tests

There are a couple of unit tests you can check by running

```sbt test```

##Implementation details

- time fragments allow either numeric templates, or string equivalents (like `feb-aug`, `mon,tue` for months and weekdays respectively)
- error handling is implemented until the first error happens, then short summary is shown in the console
- weekdays numbering starts from zero and the first weekday is Sunday (configuration could be added later)
- years part is optional; only single year, range and list are supported; years should be within (1970-9999) range