#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

CAMLprim value caml_pread(value fd, value buf, value vcount, value vfd_offset,
                          value vbuf_offset) {
  CAMLparam5(fd, buf, vcount, vfd_offset, vbuf_offset);
  long count, fd_offset;
  ssize_t ret;
  char iobuf[UNIX_BUFFER_SIZE];

  fd_offset = Long_val(vfd_offset);
  count = Long_val(vcount);
  ret = 0;
  if (count > 0) {
    count = count > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : count;
    caml_release_runtime_system();
    ret = pread(Int_val(fd), iobuf, count, fd_offset);
    caml_acquire_runtime_system();
    if (ret == -1) {
      uerror("pread", Nothing);
    }
    memmove(&Byte(buf, Long_val(vbuf_offset)), iobuf, ret);
  }
  CAMLreturn(Val_int(ret));
}
