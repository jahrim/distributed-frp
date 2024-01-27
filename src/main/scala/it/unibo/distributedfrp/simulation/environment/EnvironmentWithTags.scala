package it.unibo.distributedfrp.simulation.environment

import it.unibo.distributedfrp.frp.IncrementalCellSink
import it.unibo.distributedfrp.simulation.environment.Environment
import nz.sodium.Cell

/** An [[Environment]] where devices can be tagged with certain information. */
trait EnvironmentWithTags extends Environment:
  /**
   * @param tag the specified tag.
   * @return a [[Cell]] of all the devices tagged with the specified tag.
   */
  def withTag(tag: Any): Cell[Set[Int]]

  /**
   * Tag the specified devices with the specified tag.
   *
   * @param tag     the specified tag.
   * @param devices the specified devices.
   * @return this.
   */
  def tag(tag: Any, devices: Set[Int]): this.type
  /**
   * Remove the specified tag from the specified devices.
   *
   * @param tag     the specified tag.
   * @param devices the specified devices.
   * @return this.
   */
  def untag(tag: Any, devices: Set[Int]): this.type

  /**
   * Set the collection of devices tagged with the specified tag
   * to the specified devices.
   * In other words, remove the specified tag from all devices,
   * then tag the specified devices with the specified tag.
   *
   * @param tag     the specified tag.
   * @param devices the specified devices.
   * @return this.
   */
  def setTag(tag: Any, devices: Set[Int]): this.type
  /**
   * Remove the specified tag from all devices.
   *
   * @param tag the specified tag.
   * @return this.
   */
  def unsetTag(tag: Any): this.type = this.setTag(tag, Set.empty)
  /** As [[setTag]] but for multiple tags. */
  def setTags(tags: Iterable[(Any, Set[Int])]): this.type
  /** Alias for [[setTags]]. */
  def setTags(tags: (Any, Set[Int])*): this.type = setTags(tags)
  /**
   * Removes all tags from all devices.
   * @return this.
   */
  def unsetTags(): this.type = setTags()

/** Companion object of [[EnvironmentWithTags]]. */
object EnvironmentWithTags:
  /**
   * Decorate the specified [[Environment]] with tags.
   *
   * @param environment the specified [[Environment]].
   * @return a new [[EnvironmentWithTags]] based on the specified [[Environment]].
   */
  def apply(environment: Environment): EnvironmentWithTags =
    BasicEnvironmentWithTags(environment)

  /** Basic implementation of [[EnvironmentWithTags]]. */
  private case class BasicEnvironmentWithTags(environment: Environment)
    extends EnvironmentWithTags:
    export environment.*

    private val tagsSink: IncrementalCellSink[Map[Any, Set[Int]]] = IncrementalCellSink(Map.empty[Any, Set[Int]])
    override def withTag(tag: Any): Cell[Set[Int]] = this.tagsSink.cell.map(_.getOrElse(tag, Set.empty[Int]))
    override def setTag(tag: Any, devices: Set[Int]): this.type =
      this.tagsSink.update(_ + (tag -> devices))
      this
    override def setTags(tags: Iterable[(Any, Set[Int])]): this.type =
      this.tagsSink.set(Map.from(tags))
      this
    override def tag(tag: Any, devices: Set[Int]): this.type =
      this.tagsSink.update(tagMap => tagMap + (tag -> (tagMap.getOrElse(tag, Set.empty[Int]) ++ devices)))
      this
    override def untag(tag: Any, devices: Set[Int]): this.type =
      this.tagsSink.update(tagMap => tagMap + (tag -> (tagMap.getOrElse(tag, Set.empty[Int]) -- devices)))
      this
