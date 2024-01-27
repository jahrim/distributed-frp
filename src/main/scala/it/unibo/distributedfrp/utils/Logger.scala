package it.unibo.distributedfrp.utils

import it.unibo.distributedfrp.utils.Logger.LoggingFunction

import scala.util.{Failure, Success, Try}

/** A tool for logging messages. */
trait Logger:
  /**
   * @return the verbosity of this [[Logger]].
   *         Only messages whose verbosity is greater or equal to this
   *         value will be logged.
   */
  def verbosity: Int = Int.MaxValue

  /**
   * Log the specified message.
   *
   * @param message   the specified message.
   * @param verbosity the verbosity of the specified message. If the verbosity
   *                  of the message is lower than the verbosity of this [[Logger]],
   *                  the message won't be logged. Defaults to 1.
   * @param fn        the given [[LoggingFunction]] used to log the message.
   */
  def log(message: String, verbosity: Int = 1)(using fn: LoggingFunction = println): Unit =
    if this.verbosity >= verbosity then fn(s"$message")

  /**
   * Log the start and the potential end or failure of the specified activity.
   *
   * @param activityName the name of the specified activity.
   * @param verbosity    the verbosity of the logged messages. Defaults to 1.
   * @param activity     the specified activity.
   * @param fn           the given [[LoggingFunction]] used to log the message.
   * @tparam A the type of the result produced by the specified activity.
   * @return the result of the specified activity.
   */
  def logActivity[A](activityName: String, verbosity: Int = 1)(activity: => A)(using fn: LoggingFunction = println): A =
    this.log(s"[Start] $activityName.", verbosity)
    Try(activity) match
      case Success(result) =>
        this.log(s"[End] $activityName.", verbosity)
        result
      case Failure(exception) =>
        this.log(s"[${exception.getClass.getSimpleName}]['${exception.getMessage}'] $activityName.", verbosity)
        throw exception

/** Companion object of [[Logger]]. */
object Logger:
  /** A function that can be used to log a message. */
  type LoggingFunction = String => Unit

  /** A [[Logger]] that ignores all received messages. */
  object NoOperation extends Logger:
    override def log(message: String, verbosity: Int)(using LoggingFunction): Unit = {}

  /**
   * @param name      the specified name.
   * @param verbosity the specified verbosity level.
   * @return a new [[Logger]] with the specified name and the specified
   *         verbosity. By default, it prints the time since its creation,
   *         the name of the current thread, the name of the logger and
   *         the message to the [[Console.out stdout]].
   */
  def named(name: String, verbosity: Int = Int.MaxValue): Logger =
    NamedLogger(name, verbosity)

  /** A [[Logger]] bound to a specified name. */
  private case class NamedLogger(name: String, override val verbosity: Int) extends Logger:
    private val _initialTime: Long = System.currentTimeMillis()
    protected def prefix: String =
      s"[${System.currentTimeMillis() - this._initialTime}]" +
      s"[${Thread.currentThread().getName}]" +
      s"[$name]"

    override def log(message: String, verbosity: Int = 1)(using fn: LoggingFunction = println): Unit =
      super.log(s"$prefix: $message", verbosity)
