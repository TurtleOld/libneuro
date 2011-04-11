/* master.h */

#ifndef __MASTER_H
#define __MASTER_H

#include "common.h"

extern int Master_SetSendPacketSize(Master *msr);
extern void Master_SetQuitFlag(Master *msr);

extern void Master_AddUfds(Master *msr, Slave *slv);
extern void Master_RmUfds(Master *msr, Slave *slv);

extern void Master_PushEvent(Master *msr, Slave *slave, int sigmask);
extern void Master_EditEvent(Event *event, Slave *slave, int sigmask);

extern Status *Master_Poll(Master *msr);
extern Master *Master_Create(u32 connection_type);
extern void Master_Destroy(Master *src);

#endif /* NOT __MASTER_H */
