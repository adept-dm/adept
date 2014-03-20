package adept.test

import net.sf.ehcache.CacheManager

object LoaderUtils {
  val cacheManager = CacheManager.create //this _should_ be safe (if there is a cache issue this is not the best way to find it though) - we speed up the tests because creating the cache manager takes a while
}