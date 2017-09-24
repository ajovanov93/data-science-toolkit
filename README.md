# Data science toolkit - simple data frame library with integrated plotting.
Based on vector, labels, cassava, discrimination and chart.

Goals are to fill the gap for the library for quick exploratory data analysis like pandas(python) or data.frames(R)
More precisely the library aims to provide:
- Data frame functionality & Common functions to manipulate a dataframe. Data.Frame module
- Joins between data frames. Data.Frame.Join module (Not yet implemented)
- Some common extra functions not available in statistics package. Data.Frame.Extras module
- Enum instances for Maybe and product types. Data.Frame.Instances (In progress)
- Plotting framework inspired by ggplot (In progress)

Take a look at Main.hs for some examples of analysing the iris dataset. 

# TODO
The following things need to be added to Labels so DST can progress
- drop field from record = drop #age (#age := 1, #blabla := ":)") gives (#blabla := ":)"), project can do this but it requires a type anotation
- append two records     = append (#age := 1, #bla := ")") (#name := "User", #lbl := "Hey there") gives (#age := 1, #bla := ")", #name := "User", #lbl := "Hey there")
- easy type casts        = cast #value @Integer (#value := 1.3, #test = 1.5) = (#value := 1, #test = 1.5)  or change modify (value -> value) to (value1 -> value2)

They hold back joins, binData, mergeColumns and dropColumns
Labels.Cassava.Instances needs ToNamedRecord instances for writeCsv
