#ifndef CUBIT_FD_H
#define CUBIT_FD_H
#include <poll.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/uio.h>

/* fd.c: the descriptor table of CuBit objects. */
hidden long __cubit_fd_writev(int fd, const struct iovec *iov, int n);
hidden long __cubit_fd_read(int fd, void *buf, size_t n);
hidden long __cubit_fd_pread(int fd, void *buf, size_t n, off_t offset);
hidden long __cubit_fd_lseek(int fd, off_t offset, int whence);
hidden long __cubit_fd_open(const char *path, int flags);
hidden long __cubit_fd_close(int fd);
hidden long __cubit_fd_fstat(int fd, struct stat *st);
hidden long __cubit_fd_fcntl(int fd, int cmd, long arg);
hidden long __cubit_fd_getdents(int fd, void *buf, size_t count);
hidden long __cubit_fd_poll(struct pollfd *p, unsigned long n, unsigned long deadline);
hidden long __cubit_fd_pipe(int out[2], int flags);
hidden long __cubit_fd_socketpair(int out[2], int flags);
hidden long __cubit_fd_dup(int fd, int min, int target, int cloexec);
hidden long __cubit_path_stat(const char *path, struct stat *st);

/* file.c: filesystem.svc requests. */
hidden long __cubit_file_open(const char *path, int directory,
	uint64_t *handle, uint64_t *size);
hidden void __cubit_file_close(uint64_t handle, int directory);
hidden long __cubit_file_read_at(uint64_t handle, void *buf, size_t count,
	uint64_t offset);
hidden long __cubit_dir_read_page(uint64_t handle, void *page);
hidden void report_unsupported(const char *what, long value);

/* Readiness futex (fd.c), shared with net.c. */
hidden void __cubit_readiness_changed(void);
hidden int __cubit_readiness_seq(void);
hidden void __cubit_readiness_wait(int seq, unsigned long deadline);

/* net.c: TCP over netstack. */
struct cubit_tcp;
struct sockaddr;
hidden struct cubit_tcp *__cubit_tcp_new(void);
hidden long __cubit_tcp_connect(struct cubit_tcp *, const struct sockaddr *, unsigned, int);
hidden long __cubit_tcp_read(struct cubit_tcp *, void *, size_t, int);
hidden long __cubit_tcp_write(struct cubit_tcp *, const struct iovec *, int, int);
hidden short __cubit_tcp_poll(struct cubit_tcp *, short);
hidden long __cubit_tcp_so_error(struct cubit_tcp *);
hidden long __cubit_tcp_peer(struct cubit_tcp *, struct sockaddr *, unsigned *);
hidden long __cubit_tcp_shutdown(struct cubit_tcp *, int);
hidden void __cubit_tcp_close(struct cubit_tcp *);
hidden long __cubit_fd_socket_tcp(int flags);
hidden struct cubit_tcp *__cubit_fd_tcp(int fd, int *nonblock);
hidden int __cubit_fd_is_socket(int fd);
#endif
