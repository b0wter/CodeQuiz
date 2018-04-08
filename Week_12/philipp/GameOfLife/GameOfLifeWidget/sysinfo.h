#ifndef SYSINFO_H
#define SYSINFO_H

#include <qtypeinfo.h>

bool sysinfo_init();

double getCurrentProcessCpuValue();

quint64 getCurrentProcessRamValue();

#endif // SYSINFO_H
