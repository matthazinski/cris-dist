ifeq ($(subdir),misc)
sysdep_routines += setfsgid setfsuid setresgid setresuid
endif

ifeq ($(subdir),signal)
sysdep_routines += rt_sigsuspend rt_sigprocmask rt_sigtimedwait	\
		   rt_sigqueueinfo rt_sigaction rt_sigpending
endif

ifeq ($(subdir),sunrpc)
# Use cached copies of the rpcgen-generated files.  See sunrpc/Makefile.

# Is there a macro for the dir with the current Makefile?
target-rpcsvc-files-dir = sysdeps/unix/sysv/linux/cris/rpcsvc

ifeq (yes,$(cross-compiling))

# Add the stuff that was skipped in sunrpc/Makefile, except that the
# rpcgen program is here a script that copies a file from a directory of
# pre-generated ones.
rpcgen-cmd = $(SHELL) $(..)$(target-rpcsvc-files-dir)/../fake-rpcgen-sh $(..)$(target-rpcsvc-files-dir)

headers += $(rpcsvc:%.x=rpcsvc/%.h)
extra-libs := librpcsvc
extra-libs-others := librpcsvc # Make it in `others' pass, not `lib' pass.
librpcsvc-routines = $(rpcsvc:%.x=x%)
librpcsvc-inhibit-o = .os # Build no shared rpcsvc library.
omit-deps = $(librpcsvc-routines)

# Add dependencies to just cause failure (no command to update) if the
# .x is newer than the target-specific .h or .c when cross-compiling.
$(rpcsvc:%.x=$(objpfx)rpcsvc/%.stmp): $(objpfx)rpcsvc/%.stmp: $(..)$(target-rpcsvc-files-dir)/%.x.h
$(rpcsvc:%.x=$(objpfx)x%.stmp): $(objpfx)x%.stmp: $(..)$(target-rpcsvc-files-dir)/%.x.c

$(rpcsvc:%.x=$(..)$(target-rpcsvc-files-dir)/%.x.h): $(..)$(target-rpcsvc-files-dir)/%.x.h: rpcsvc/%.x
	# OUT-OF-SYNC: $@ out of date compared to $<: need to regenerate manually.
	exit 1

$(rpcsvc:%.x=$(..)$(target-rpcsvc-files-dir)/%.x.c): $(..)$(target-rpcsvc-files-dir)/%.x.c: rpcsvc/%.x
	# OUT-OF-SYNC: $@ out of date compared to $<: need to regenerate manually.
	exit 1
endif
endif
