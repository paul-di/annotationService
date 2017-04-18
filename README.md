## AnnotationService
### Task description
You should write a REST servise that annotates text with information about cities. Information about cities can be get here [cities15000](http://download.geonames.org/export/dump/). Service should support matching by name and alternative names, exact and lowercased match (configurable). Service should return results in JSON or XML format. Example: 
```
val text = "NeW YoRk CiTy is the most populous..."
```
```
matches : {
  match : {
    geoid : NUMBER,
    name : "New York City",
    covered_text : "NeW YoRk CiTy",
    start : 0,
    end : 13, (java  style exclusive)
    score : 1.0
  } 
}  
```
### Assumptions/questions
  1. How to evaluate situaltions when several matches possible? For example: *New York City* also can be matched as *York*. Assumption: completely nested matches should be ignored, partially nested will both appear in result set. So we prefer longest matches over short (it can cause problems sometimes)
  2. Every match should starts at some token start and ends at some token end. To prevent *New Yorker* from matching to *New York*. So we need at least some kind of dummy tokenizer
  3. Task doesn't include wordforms, morfology: only exact and lowercase match. So we can use [Aho–Corasick algorithm](https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm)
