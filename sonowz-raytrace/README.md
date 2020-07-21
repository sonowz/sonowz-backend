# Raytrace Demo

Self-study & Computer graphics class raytrace assignment demonstration

It takes some time to process image, so message queue was implemented.

Assignment code not included.

http://sonowz.me/raytrace

## System Architecture
![image](https://github.com/sonowz/sonowz-backend/blob/master/sonowz-raytrace/SystemArchitecture.png)

[edit link](https://app.cloudcraft.co/view/90ca36c3-b22b-4112-8b5e-d9420dcdee19?key=7hnJeHCJhj1sj34R8mcrig)

#### Used:
- PostgreSQL for simple message queue
- Websocket for receiving queue status

## TODO
- Purchase domain
- Pretty web page

## Refactoring ideas
- Split `App/Daemon/Process.hs` and `App/Web/Websocket.hs` by thread
- Split Daemon and Web process as separate binary
- Use `ekg` library
- Make `timeToIOFinal` (this might eliminate the need to do second idea)
