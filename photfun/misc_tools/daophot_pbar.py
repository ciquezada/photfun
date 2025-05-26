import time
from datetime import timedelta


def daophot_pbar(pbar=None, func_msg="Processing"):
    """
    Returns a generator wrapper that works with enumerate.

    Example:
    with ui.Progress(min=0, max=len(my_list)) as p:
        for i, item in enumerate(daophot_pbar(p, "Processing")(my_list)):
            ...
    """
    def yielder(iterable):
        start_time = time.time()

        try:
            total = len(iterable)
        except TypeError:
            total = None

        counter = 0
        pbar.set(message=f"Executing {func_msg}", detail="Starting...")

        for item in iterable:
            yield item
            counter += 1

            elapsed_time = time.time() - start_time
            time_per_iter = elapsed_time / counter
            remaining_time = (time_per_iter * (total - counter)) if total else 0
            remaining_time_str = (
                str(timedelta(seconds=int(remaining_time)))
                if total and counter > 1 else "Estimating..."
            )

            if total:
                amount = 1 / total
                pbar.inc(amount,
                         message=f"{func_msg}",
                         detail=f"Progress: {counter}/{total} | Time left: {remaining_time_str}")
            else:
                pbar.inc(0,
                         message=f"{func_msg}",
                         detail=f"Items processed: {counter} | Elapsed: {str(timedelta(seconds=int(elapsed_time)))}")

    return yielder