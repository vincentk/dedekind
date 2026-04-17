# Module Header Quotes: Cultural Embedding Guidelines

Module header quotes in the dedekind project serve to anchor mathematical concepts in their historical and geographic context. The selection process deliberately emphasizes that mathematics is a **global, centuries-long endeavor** reflecting contributions from practitioners across diverse cultures and traditions.

## Selection Methodology

When choosing a quotation for a new module header, follow these steps:

### 1. **Identify the Domain**
   - Clarify the mathematical concept(s) central to the module (e.g., "inner products," "linear transformations," "Hilbert space completion").
   - This anchors your search to relevant practitioners.

### 2. **Research Key Practitioners**
   - Identify founders and major contributors to the domain:
     - **Founders**: The mathematicians who introduced or formalized the core concept.
     - **Recent experts**: Contemporary or modern practitioners who deepened understanding.
   - **Flexibility note**: While mathematicians and domain practitioners are preferred, the quotation does not have to come from a mathematician. It only needs to be on topic and illuminate the mathematical domain. This can include philosophers, scientists, artists, fictional characters, or other thinkers whose words resonate with the module's concepts.
   - Aim for **geographic and cultural diversity** across the module headers:
     - Consider nationality, region of work, cultural context, historical era.
     - Avoid clustering all quotes from a single geographic region or era.

### 3. **Determine Native Language**
   - For each candidate practitioner, identify their **native language** (or primary publication language if not English).
   - Check primary sources: Wikipedia biography, mathematical encyclopedias, academic databases.
   - This step is crucial: we want to present quotes in their **native form**, not always in English translation.

### 4. **Source the Original Quote**
   - **Search primary works** (original publications, books, collected works) in the native language.
   - Use secondary sources (Wikiquote, Wikisource) to narrow down which work contains a relevant passage, but verify against the primary work when possible.
   - Record the exact source: author, work title, year, chapter/section (if applicable), and page number if available.

### 5. **Provide Both Languages**
   - Present the quote in **both** native language (if non-English) and English translation.
   - Format:
     ```cpp
     @note "[NATIVE LANGUAGE QUOTE]"
     *       -- [Name], [Work] ([Year]) [Section/Chapter]
     *
     *       English translation: "[ENGLISH QUOTE]"
     ```
   - This honors the original context while ensuring accessibility.

### 6. **Ensure Relevance and Pertinence**
   - The quote should illuminate an aspect of the domain that resonates with the module's mathematical content.
   - Avoid overly famous or generic quotes; seek "serendipitous" connections that reveal the practitioner's intuition about the field.

## Examples

### Inner Product Module
- **Domain**: Inner products, metric properties of angles, pre-Hilbert spaces.
- **Practitioner**: Henri Poincaré (French mathematician, 1854–1912).
- **Native language**: French.
- **Primary source**: *Science et méthode* (1908), Livre II, § II ("Les définitions mathématiques et l'Enseignement").
- **Quote form**: French original + English translation.

### Hilbert Space Module
- **Domain**: Hilbert spaces, completeness, infinite-dimensional geometry.
- **Practitioner**: Hermann Weyl (German mathematician, 1885–1955).
- **Native language**: English (major works published in English during his Princeton years).
- **Primary source**: *Symmetry* (1952), Princeton University Press.
- **Quote form**: English original with full citation.

## Implementation Notes

- **Attribution accuracy**: Always cite the full publication, not just the author and year.
- **Translation credit**: If using a non-original translation, note the translator if known.
- **Verification**: Use at least two independent sources (primary work + Wikiquote or similar) to confirm the exact wording.
- **Formatting**: Follow the established Doxygen comment format in `@note` blocks; include module author names and edition information for clarity.

## Principles

1. **Authenticity**: Quotes appear in their native language first, honoring the original context.
2. **Inclusivity**: Actively seek practitioners from underrepresented regions and eras.
3. **Rigor**: Source quotes from primary works; avoid third-hand attributions.
4. **Accessibility**: Always provide English translation for non-English quotes.
5. **Durability**: Document the source thoroughly so future contributors can verify and understand the selection.
