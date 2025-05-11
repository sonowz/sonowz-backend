# Raytrace Demo

Self-study & Computer graphics class raytrace assignment demonstration

It takes some time to process image, so message queue was implemented.

Ray tracing assignment code is not included. This code just manages websocket connection with client.

https://sonowz.me/raytrace

## System Architecture
![image](https://github.com/sonowz/sonowz-backend/blob/master/sonowz-raytrace/SystemArchitecture.png)

[edit link](https://app.cloudcraft.co/view/90ca36c3-b22b-4112-8b5e-d9420dcdee19?key=7hnJeHCJhj1sj34R8mcrig)

#### Used:
- PostgreSQL for simple message queue
- Websocket for receiving queue status

## TODO
- Pretty web page

## Refactoring ideas
- Split `App/Daemon/Process.hs` and `App/Web/Websocket.hs` by thread
- Use `ekg` library?

## Bugs
- Websocket sends ProcessStarted to client after ProcessFinished
    - Split Daemon and Web process as separate binary
    - Using `timeToIOFinalFinal` might fix the bug
