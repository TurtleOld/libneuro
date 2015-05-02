/* parseConf.h */

#ifndef __PARSECONF_H
#define __PARSECONF_H

#include <neuro/NEURO.h>

typedef struct ConfigData ConfigData;
typedef struct ConfigDatas ConfigDatas;


/* returns 1 on error and 0 if all is ok 
 * don't free paramName or paramValue and
 * don't change their values either
 */
extern int Neuro_GiveNextConfig(ConfigDatas *c, char **paramName, char **paramValue);

/* resets the positional pointer to the first one (this is to be used with Neuro_GiveNextConfig) */
extern void Neuro_ResetConfig(ConfigDatas *c);

/* please treat the result as read-only and don't free it */
extern char *Neuro_ConfigSearchValueByName(ConfigDatas *c, const char *paramName);

/* This is the main function to parse configuration files */
extern ConfigDatas *Neuro_ParseConf(const char *filePath);

extern void Neuro_CleanConfigDatas(ConfigDatas *c);

/* internal only */

extern int ParseConf_Init();
extern void ParseConf_Clean();

#endif /* NOT __PARSECONF_H */
