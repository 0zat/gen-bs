
let types_in_bs_dom = [
  "_baseClass";
  "animation";
  "cssStyleDeclaration";
  "cssStyleSheet";
  "eventTarget_like";
  "eventTarget";
  "_node";
  "node_like";
  "node";
  "_attr";
  "attr";
  "_characterData";
  "characterData_like";
  "characterData";
  "_cdataSection";
  "cdataSection";
  "_comment";
  "comment";
  "_document";
  "document_like";
  "document";
  "_documentFragment";
  "documentFragment";
  "_documentType";
  "documentType";
  "domImplementation";
  "_element";
  "element_like";
  "element";
  "htmlCollection";
  "mutationObserver";
  "mutationRecord";
  "namedNodeMap";
  "nodeList";
  "processingInstruction";
  "_shadowRoot";
  "shadowRoot";
  "_text";
  "text";
  "domRect";
  "dataTransfer";
  "domStringMap";
  "history";
  "_htmlDocument";
  "htmlDocument";
  "_htmlElement";
  "htmlElement_like";
  "htmlElement";
  "_htmlSlotElement";
  "htmlSlotElement";
  "location";
  "window";
  "_xmlDocument";
  "xmlDocument";
  "event_like";
  "event";
  "_uiEvent";
  "uiEvent_like";
  "uiEvent";
  "_animationEvent";
  "animationEvent";
  "_beforeUnloadEvent";
  "beforeUnloadEvent";
  "_clipboardEvent";
  "clipboardEvent";
  "_closeEvent";
  "closeEvent";
  "_compositionEvent";
  "compositionEvent";
  "_customEvent";
  "customEvent";
  "_dragEvent";
  "dragEvent";
  "_errorEvent";
  "errorEvent";
  "_focusEvent";
  "focusEvent";
  "_idbVersionChangeEvent";
  "idbVersionChangeEvent";
  "_inputEvent";
  "inputEvent";
  "_keyboardEvent";
  "keyboardEvent";
  "_mouseEvent";
  "mouseEvent_like";
  "mouseEvent";
  "_pageTransitionEvent";
  "pageTransitionEvent";
  "_pointerEvent";
  "pointerEvent";
  "_popStateEvent";
  "popStateEvent";
  "_progressEvent";
  "progressEvent";
  "_relatedEvent";
  "relatedEvent";
  "_storageEvent";
  "storageEvent";
  "_svgZoomEvent";
  "svgZoomEvent";
  "_timeEvent";
  "timeEvent";
  "_touchEvent";
  "touchEvent";
  "_trackEvent";
  "trackEvent";
  "_transitionEvent";
  "transitionEvent";
  "_webGlContextEvent";
  "webGlContextEvent";
  "_wheelEvent";
  "wheelEvent";
  "range";
  "selection";
  "domTokenList";
  "domSettableTokenList";
  "nodeFilter";
  "nodeIterator";
  "treeWalker";
  "svgRect";
  "svgPoint";
  "eventPointerId"
]

let types_in_bs_dom = []

let type_names_bs_dom =
  List.map (fun x -> (["Dom"], x)) types_in_bs_dom

let types =
  let lowers = List.map String.lowercase_ascii types_in_bs_dom in
  let type_names = type_names_bs_dom in
  List.combine lowers type_names

let find name =
  match List.assoc (String.lowercase_ascii name) types with
  | dom -> Some dom
  | exception Not_found -> None