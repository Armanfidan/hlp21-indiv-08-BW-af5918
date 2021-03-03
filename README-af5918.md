# ISSIE draw implementation - BusWire - Arman Fidanoglu, CID 01512561
#### Link to group GitHub repo: https://github.com/lb4418/hlp21-team8

I have implemented BusWire and integrated with my teammate Lukas Baliunas's Sheet. I have also modified Symbol to work well with my and Lukas's implementations.

List of features:
- Autorouted wires with variable number of segments (3, 5 or 7 depending on source and target port positions)
- Better autorouting than current ISSIE (wires do not go over symbols, this is accomplished by inferring wire corners from bounding boxes of host symbols of ports)
- Wire width inference (Depending on the source and target ports, which have a property "Width" as defined in group interface documentation)
- Width mismatch error recognition
- Wire selection (multiple wire selection supported)
- Wire manual routing (based on a list of bounding boxes of all segments of the wires. Manual routing resets back to automatic upon dragging of a symbol)
- Wire deselection (by clicking on the canvas or another symbol or wire)
- Wire addition
- Wire deletion (multiple wire deletion supported)
- Symbol deletion (upon deleting a symbol, all wires emanating from or ending at the symbol are safely deleted)
