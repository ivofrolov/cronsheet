# Cron visual schedule of a kind

Go to <https://ivofrolov.github.io/cronsheet/> and paste your crontab.

Or you can clone the repo and open `index.html` by hand. No server needed.

### What

It provides overview of "busy" slots over a day.

Given a crontab

```
# m h dom mon dow user  command
 30 7  *   *   *  user  make coffee
  * *  *   *   *  user  fping localhost
  5 *  *   *   *  root  logrotate
```

and a grid granularity of 15 minutes, schedule will look something like this.

```
Command / Time, h.  00  01  02  03  04  05  06  07  08  09  10  11  12   ~ 23
make coffee                                       ▒                      ~
fping localhost     ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ~ ▒▒▒▒
logrotate           ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒    ~ ▒
                                                ^
                                                07:00 to 07:15 slot
```
