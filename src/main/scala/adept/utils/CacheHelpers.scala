package adept.utils

import net.sf.ehcache.Ehcache
import net.sf.ehcache.Element

object CacheHelpers {
  def usingCache[T](key: String, cache: Ehcache)(block: => T): T = {
    val cachedValues = cache.get(key)
    if (cache.isKeyInCache(key) && cachedValues != null) {
      cachedValues.getObjectValue().asInstanceOf[T]
    } else {
      val result = block
      val element = new Element(key, result)
      cache.put(element)
      result
    }
  }
}