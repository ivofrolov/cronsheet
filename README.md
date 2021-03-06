# Cron visual schedule of a kind

Provides overview of "busy" slots over a day.

### Example

Given a crontab

```
# m h dom mon dow user  command
 30 7  *   *   *  user  make coffee
  * *  *   *   *  user  fping localhost
  5 *  *   *   *  root  logrotate
```

And a grid granularity of 15 minutes, it looks something like this.

```
Command / Time, h.  00  01  02  03  04  05  06  07  08  09  10  11  12   ~ 23  
make coffee                                       ▒                      ~     
fping localhost     ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ~ ▒▒▒▒
logrotate           ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒   ▒    ~ ▒   
                                                ^
                                                07:00 to 07:15 slot
```
