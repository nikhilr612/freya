//! Module to provide a convenience function for executing embarrassingly parallel tasks.

use log::{debug, info, warn};
use std::{sync::mpsc, thread::available_parallelism};

/// Apply a function or closure to the elements of a vector in parallel, and return the corresponding vector of results.
pub fn apply_parallel<T, F, E>(pname: &str, list: Vec<T>, op: F) -> Vec<E>
where
    F: (FnOnce(T) -> E) + Send + 'static + Copy,
    T: Send + 'static,
    E: Send + 'static,
{
    if list.is_empty() {
        return Vec::new();
    }

    let n_jobs = list.len();
    let n_workers = n_jobs.min(
        available_parallelism()
            .expect("Failed to obtain available parallel compute / parallelism.")
            .into(),
    );

    debug!("Using {n_workers} workers for {n_jobs} jobs.");
    let pool = threadpool::Builder::new()
        .num_threads(n_workers)
        .thread_name(pname.to_owned())
        .build();

    let (tx, rx) = mpsc::channel();

    for elm in list {
        let ltx = tx.clone();
        pool.execute(move || {
            ltx.send(op(elm))
                .expect("Failed to send result message to receiver channel.")
        })
    }

    // Wait for all jobs to finish.
    let v: Vec<_> = rx.iter().take(n_jobs).collect();

    let n_succ = v.len();
    if n_succ < n_jobs {
        warn!("Some jobs have terminated abruptly.");
    }
    info!(
        "Completed {n_succ}/{n_jobs} jobs ({:.3}%).",
        (n_succ as f64) / (n_jobs as f64) * 100.0
    );

    v
}
