#include <ntddk.h>
#ifndef NDEBUG
#define NDEBUG
#endif
#include <debug.h>
#include <ntifs.h>
#include <ndk/ntndk.h>
#include <windef.h>


DRIVER_UNLOAD DriverUnload;
VOID NTAPI DriverUnload(IN PDRIVER_OBJECT DriverObject)
{
    DPRINT1("------------------DRIVER-UNLOADED--------------------\n");
    IoDeleteDevice(DriverObject->DeviceObject);
}

NTSTATUS NTAPI DriverEntry(IN PDRIVER_OBJECT DriverObject,
            IN PUNICODE_STRING RegistryPath)
{
    DPRINT1("--------Okutin Denis lab4--------\n");
    DriverObject->DriverUnload = DriverUnload;
    MmPageEntireDriver(DriverEntry);


    PVOID Pages = NULL;
    PHARDWARE_PTE PTE_BASE = (PHARDWARE_PTE)0xc0000000;
    PHARDWARE_PTE pte;
    SIZE_T sizeReserve = PAGE_SIZE * 10; 
    SIZE_T sizeCommit = PAGE_SIZE * 5;
    int i;

    ZwAllocateVirtualMemory(NtCurrentProcess(), &Pages, 0, &sizeReserve, MEM_RESERVE, PAGE_READWRITE);
    DPRINT1("10 PAGES RESERVED\n");

    ZwAllocateVirtualMemory(NtCurrentProcess(), &Pages, 0 , &sizeCommit, MEM_COMMIT, PAGE_READWRITE);
    DPRINT1("5 PAGES COMMITED\n");


    for(i = 0; i < 5; i++) {
        *((PCHAR)Pages + 0x1000 * i) = i + 1;
    }


    for(i = 0; i < 5; i++) {
        pte = ((ULONG)Pages >> 12) + PTE_BASE + i;
        DPRINT1("Page: %d\n\
        Physical address: %X\n\
        Valid:            %d\n\
        Accessed:         %d\n\
        Dirty:            %d\n\
        \n", i + 1, pte->PageFrameNumber<<12, pte->Valid,
         pte->Accessed,
        pte->Dirty);
    }
    ZwFreeVirtualMemory(NtCurrentProcess(), &Pages, &sizeCommit, MEM_DECOMMIT);
    DPRINT1("MEMORY IS DECOMMITED\n");
    ZwFreeVirtualMemory(NtCurrentProcess(), &Pages, 0, MEM_RELEASE);
    DPRINT1("MEMORY IS RELEASED\n");

    return STATUS_SUCCESS;
}
