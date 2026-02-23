# what-if

What if my main language had very little written material? What if there were few books, few classes, few references, and yet it was the language that touches my heart?

This project began there.

It is an interactive map of Indigenous languages spoken in Brazil, based on data from the 2022 Demographic Census conducted by the Instituto Brasileiro de Geografia e Estatística (IBGE), built with R and Leaflet.

---

## what this map shows

For each Brazilian municipality, the map displays:

* Total resident population
* Indigenous resident population
* Proportion of Indigenous language speakers aged 15+
* The three most spoken Indigenous languages declared in the municipality
* The share of the Indigenous population that speaks each of those languages

The visualization is interactive.
Hovering highlights a municipality.
Clicking reveals its linguistic scenario.

The color scale reflects the proportion of Indigenous language speakers among residents aged 15 or older.

## how it was built

All data come from the 2022 Census, including:

* Appendix 02 – Linguistic trunks, families, and languages
* Complementary Table 14 – Indigenous language speakers (15+)
* Complementary Table 26 – Languages spoken at home (2+)
* Population tables 9514 and 9718

Municipal boundaries were obtained through the geobr package.

The workflow includes:

* Cleaning footnotes embedded in language names
* Harmonizing language-family classifications
* Removing single-speaker cases to reduce statistical noise
* Selecting the three most spoken Indigenous languages per municipality
* Cross-validating published proportions with recomputed ones
* Constructing structured HTML popups for contextual clarity

The map was built using leaflet in R.

## why this exists

While reading about efforts to translate materials into Ticuna — one of the most spoken Indigenous languages in Brazil — I found myself asking a simple question:

What if it were mine?

What if my language did not dominate publishing, education, software, or search engines?

This project does not answer that question.

It just makes something visible.

## limitations

Census data reflect self-declaration and reporting structures.
Languages may be underreported, grouped, or differently classified across sources.
The map represents distribution — not vitality, transmission, or cultural continuity.

Numbers are not the language.

They are only traces of it.

## next steps

Future developments may include:

* Exploring demographic composition of speakers
* Comparing literacy patterns among Indigenous language speakers
* Investigating linguistic family distribution spatially
* Examining proportional presence relative to total municipal population

For now, this is a first version.

A map.
A question.
A beginning.
