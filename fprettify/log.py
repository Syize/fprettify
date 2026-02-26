import logging


def set_fprettify_logger(level):
    """setup custom logger"""
    logger = logging.getLogger("fprettify-logger")
    logger.setLevel(level)
    stream_handler = logging.StreamHandler()
    stream_handler.setLevel(level)
    formatter = logging.Formatter(
        "%(levelname)s: File %(ffilename)s, line %(fline)s\n    %(message)s"
    )
    stream_handler.setFormatter(formatter)
    logger.addHandler(stream_handler)


def log_exception(e, message, level="exception"):
    """log an exception and a message"""
    log_message(message, level, e.filename, e.line_nr)


def log_message(message, level, filename, line_nr):
    """log a message"""

    logger = logging.getLogger("fprettify-logger")
    logger_d = {"ffilename": filename, "fline": line_nr}
    logger_to_use = getattr(logger, level)
    logger_to_use(message, extra=logger_d)


__all__ = ["set_fprettify_logger", "log_exception", "log_message"]
