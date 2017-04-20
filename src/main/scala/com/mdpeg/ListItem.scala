package com.mdpeg

sealed trait ListItem

final case class OrderedListItem(inline: String) extends ListItem

final case class UnorderedListItem(inline: String) extends ListItem
