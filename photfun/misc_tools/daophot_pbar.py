import time
from datetime import timedelta

def daophot_pbar(pbar=None, func_msg="Processing", *args, **kwargs):
    """
    This function is intended to transform a progress_bar of shiny
    in something similar as tqdm with the following command line
    
    with ui.Progress(min=0, max=max_n) as p:
        pbar = daophot_pbar(p, "message")
        for var in pbar(foo_list):
            var
    """
    start_time = time.time()
    def yielder(iterable):
        # Modo iterable directo
        counter = 0
        total = len(iterable)
        amount = 1/total
        pbar.set(message=f"Executing {func_msg}", detail="Starting...")
        
        for item in iterable:
            yield item
            counter += 1
            
            elapsed_time = time.time() - start_time
            time_per_iter = elapsed_time / counter
            remaining_time = time_per_iter * (total - counter)
            
            remaining_time_str = (str(timedelta(seconds=int(remaining_time))) 
                                if counter > 1 else "Estimating...")
            
            pbar.inc(amount,
                    message=f"{func_msg}",
                    detail=f"Progress: {counter}/{total} | Time left: {remaining_time_str}")
    return yielder