window.setupDraggable = function setupDraggable(sendEvent) {
  const MINIMUM_DRAG_DISTANCE_PX = 10;

  // html attributes
  const DRAG_BEACON_ATTRIBUTE = "drag-beacon";
  const DROP_BEACON_ATTRIBUTE = "drop-beacon";
  const HOVER_BEACON_ATTRIBUTE = "hover-beacon";

  // browser events
  const POINTER_DOWN = "pointerdown";
  const POINTER_MOVE = "pointermove";
  const POINTER_UP = "pointerup";

  // elm events
  const MOVE = "move";
  const STOP = "stop";
  const START = "start";
  const HOVER = "hover";

  document.addEventListener(POINTER_DOWN, awaitDragStart);
  document.addEventListener(POINTER_MOVE, sendHoverEvent);

  function awaitDragStart(startEvent) {
    let startBeaconId = null;
    let cursorOnDraggable = null;
    const startBeaconElem = startEvent.target.closest(
      `[${DRAG_BEACON_ATTRIBUTE}]`
    );
    if (startBeaconElem) {
      startBeaconId = startBeaconElem.getAttribute(DRAG_BEACON_ATTRIBUTE);
      const { left, top } = startBeaconElem.getBoundingClientRect();
      cursorOnDraggable = {
        x: startEvent.clientX - left,
        y: startEvent.clientY - top,
      };
    }

    changeMoveListener(sendHoverEvent, maybeDragMove);
    document.addEventListener(POINTER_UP, stopAwaitingDrag);

    function stopAwaitingDrag() {
      changeMoveListener(maybeDragMove, sendHoverEvent);
      document.removeEventListener(POINTER_UP, stopAwaitingDrag);
    }

    function maybeDragMove(moveEvent) {
      const dragDistance = distance(coords(startEvent), coords(moveEvent));
      if (dragDistance >= MINIMUM_DRAG_DISTANCE_PX) {
        sendStartEvent(startEvent, startBeaconId, cursorOnDraggable);
        sendMoveEvent(moveEvent);

        changeMoveListener(maybeDragMove, sendMoveEvent);
        document.removeEventListener(POINTER_UP, stopAwaitingDrag);
        document.addEventListener(POINTER_UP, dragEnd);
      }
    }
  }

  function dragEnd(event) {
    sendStopEvent(event);
    changeMoveListener(sendMoveEvent, sendHoverEvent);
    document.removeEventListener(POINTER_UP, dragEnd);
  }

  // START should include only DRAG beacons
  function sendStartEvent(event, startBeaconId, cursorOnDraggable) {
    sendEvent({
      type: START,
      cursor: coords(event),
      beacons: beaconPositions(DRAG_BEACON_ATTRIBUTE),
      startBeaconId,
      cursorOnDraggable,
    });
  }

  // STOP should include only DROP beacons
  function sendStopEvent(event) {
    sendEvent({
      type: STOP,
      cursor: coords(event),
      beacons: beaconPositions(DROP_BEACON_ATTRIBUTE),
    });
  }

  // MOVE should include only DROP beacons
  function sendMoveEvent(event) {
    sendEvent({
      type: MOVE,
      cursor: coords(event),
      beacons: beaconPositions(DROP_BEACON_ATTRIBUTE),
    });
  }

  // HOVER should include only HOVER beacons
  function sendHoverEvent(event) {
    sendEvent({
      type: HOVER,
      cursor: coords(event),
      beacons: beaconPositions(HOVER_BEACON_ATTRIBUTE),
    });
  }

  function changeMoveListener(from, to) {
    document.removeEventListener(POINTER_MOVE, from);
    document.addEventListener(POINTER_MOVE, to);
  }

  function beaconPositions(attr) {
    const beaconElements = document.querySelectorAll(`[${attr}]`);
    return Array.from(beaconElements).map((elem) => beaconData(elem, attr));
  }

  function beaconData(elem, attr) {
    const boundingRect = elem.getBoundingClientRect();
    const beaconId = elem.getAttribute(attr);
    return {
      id: tryParse(beaconId),
      x: boundingRect.x,
      y: boundingRect.y,
      width: boundingRect.width,
      height: boundingRect.height,
    };
  }

  function tryParse(str) {
    try {
      return JSON.parse(str);
    } catch (e) {
      return str;
    }
  }

  function coords(event) {
    return { x: event.clientX, y: event.clientY };
  }

  function distance(pos1, pos2) {
    const dx = pos1.x - pos2.x;
    const dy = pos1.y - pos2.y;
    return Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
  }
};
