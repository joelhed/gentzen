## Proof of concept

- [x] Render proofs
  - [x] Change the elm rendering to use a separate html file for custom CSS
  - [x] Extract styling into a separate CSS file
  - [x] Make the drawing area a bit more distinct and centered
  - [x] Center the proof
  - [x] Make sure the horizontal line between the premises and the conclusion only
        extend to the edges of the farthest out premises
        - This is semi-done/done for now. Turns out it's pretty hard to do well. I took inspiration
          from [this Medium article](https://medium.com/@ross.angus/sitemaps-and-dom-structure-from-nested-unordered-lists-eab2b02950cf), but using that doesn't align the line with the statements perfectly, but it trims down the edges a bit. (2020-05-02)
- [ ] Make proofs editable
  - [x] Editable statements
        - Visually, this isn't very pleasing. The abstraction level might also be
          completely off, but we'll get to that when we get to it. (2020-05-02)
  - [x] Premises (add and remove)
  - [ ] Subproofs (add and remove)
  - [ ] Discharge brackets (add and remove)
- [ ] Improve UX

## Potential future features

- [ ] Make proofs draggable
  - [ ] Add a 2x3 dotted handle to the left of the "deduction line" that can be dragged
        to move the proof
  - [ ] Drop a proof into a premise spot
  - [ ] Right click on a blank space to create a new proof at that location
- [ ] Transform the text to use proper logic symbols
- [ ] Undo/redo
- [ ] Server
  - [ ] Exercises
  - [ ] Parsing proofs and checking their validity
  - [ ] User management
  - [ ] Exercise answer submissions
