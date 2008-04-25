#include "input_node_data_writer_tasklet.h"

Input_node_data_writer_tasklet::
Input_node_data_writer_tasklet() {

}

Input_node_data_writer_tasklet::~Input_node_data_writer_tasklet() {
  if (input_buffer_ != Input_buffer_ptr()) {
    if (!input_buffer_->empty()) {
      DEBUG_MSG("There is still data to be written. "
                << input_buffer_->size());
    }
  }
  while  (!data_writers_.empty()) {
    if ((data_writers_.front().writer->get_size_dataslice() <= 0) &&
        (data_writers_.front().slice_size == 0)) {
      data_writers_.pop();
    } else {
      break;
    }
  }
  if (!data_writers_.empty()) {
    DEBUG_MSG("Data_writers are still waiting to produce output.");
  }
}

void
Input_node_data_writer_tasklet::
connect_to(Input_buffer_ptr new_input_buffer) {
  input_buffer_ = new_input_buffer;
}

bool
Input_node_data_writer_tasklet::
has_work() {
  if (input_buffer_->empty())
    return false;

  if (data_writers_.empty())
    return false;

  if (!data_writers_.front().writer->can_write()) {
    //DEBUG_MSG("Data writer can't write");
    return false;
  }
  return true;
}

void
Input_node_data_writer_tasklet::
do_task() {
  // Header contains
  // - int32_t invalid_samples_begin
  // - int32_t nr_invalid_samples
  // - char offset (in samples within the first byte)
  // The data contains nr_channels/samples_per_byte+1 bytes

  assert(has_work());

  Input_buffer_element &input_element = input_buffer_->front();

  // Check if we only need to release the block
  if (input_element.release_data) {
    input_element.channel_data.release();
    input_buffer_->pop();
    return;
  }

  // Check whether we have to start a new timeslice
  if (data_writers_.front().slice_size > 0) {
    // Initialise the size of the data slice
    assert(data_writers_.front().writer->get_size_dataslice() <= 0);
    int nr_bytes = data_writers_.front().slice_size;
    assert(nr_bytes != 0);
    data_writers_.front().writer->set_size_dataslice(nr_bytes);
    data_writers_.front().slice_size = 0;
  }
  assert(data_writers_.front().writer->get_size_dataslice() >= 0);
  if (data_writers_.front().writer->get_size_dataslice() == 0) {
    data_writers_.pop();
    return;
  }

  if ((int)input_element.delay >= 0) {
    assert((input_element.invalid_samples_begin >= 0) &&
           (input_element.invalid_samples_begin <= 1024));
    assert((input_element.nr_invalid_samples >= 0) &&
           (input_element.nr_invalid_samples <= 1024));
    int nbytes = 0;

    // write the information on invalid samples
    nbytes = data_writers_.front().writer->put_bytes(sizeof(input_element.invalid_samples_begin),
                                                     (char*)&input_element.invalid_samples_begin);
    assert(nbytes == sizeof(input_element.invalid_samples_begin));
    nbytes = data_writers_.front().writer->put_bytes(sizeof(input_element.nr_invalid_samples),
                                                     (char*)&input_element.nr_invalid_samples);
    assert(nbytes == sizeof(input_element.nr_invalid_samples));

    do {
      nbytes = data_writers_.front().writer->put_bytes(1, &input_element.delay);
    } while (nbytes != 1);
  }

  int bytes_to_write = input_element.nr_bytes;
  int bytes_written = 0;
  char *data =
    (char*)&input_element.channel_data.data().data[input_element.first_byte];

  while (bytes_written < bytes_to_write) {
    int nbytes =
      data_writers_.front().writer->put_bytes(bytes_to_write - bytes_written,
                                              data);
    assert(nbytes >= 0);
    bytes_written += nbytes;
    data          += nbytes;
  }

  input_buffer_->pop();
}

void
Input_node_data_writer_tasklet::
add_data_writer(Data_writer_ptr data_writer, int nr_bytes) {
  Writer_struct writer;
  writer.writer = data_writer;
  writer.slice_size = nr_bytes;
  data_writers_.push(writer);
}

void
Input_node_data_writer_tasklet::
set_parameters(const Input_node_parameters &input_param) {}
