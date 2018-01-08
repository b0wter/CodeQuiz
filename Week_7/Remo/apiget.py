from urllib.request import HTTPError, urlopen
from json import loads, dumps
import logging

LOGBASE = "api"
LOGLEVEL = logging.INFO


class APIBase(object):
    URL = "http://cqbsapiquiz.azurewebsites.net/api/values"

    def __init__(self):
        self.logger = logging.getLogger(LOGBASE + "." + self.name)

    @property
    def name(self):
        return self.__class__.__name__

    @property
    def url(self):
        return APIBase.URL + "/" + self.name

    def urlId(self, idn):
        if self.name == "CEntity":
            return APIBase.URL + "/" + str(idn) + "/" + self.name
        else:
            return self.url + "/" + str(idn)

    def list(self):
        self.logger.info("getting listing")
        self.logger.debug("requesting " + self.url)
        with urlopen(self.url) as result:
            self.logger.info("request returned %i", result.code)
            data = result.readline()
            self.logger.debug("read %i bytes", len(data))
        return loads(data)

    def get(self, idn):
        url = self.urlId(idn)
        self.logger.info("getting ID %i", idn)
        self.logger.debug("requesting " + url)

        try:
            with urlopen(url) as result:
                self.logger.info("request returned %i", result.code)
                if result.code != 200:
                    return self.get(idn)
                data = result.readline()
                self.logger.debug("read %i bytes", len(data))
        except HTTPError as exc:
            self.logger.error("HTTPError: %s, retrying", exc.msg)
            return self.get(idn)

        obj = loads(data)
        if "ErrorCode" in obj.keys():
            self.logger.warn("ErrorCode: %i, Description: %s", obj["ErrorCode"], obj["Description"])
            if obj["Description"].endswith("erneut versuchen."):
                self.logger.info("retrying")
                return self.get(idn)
            else:
                return None
        return obj


class Foo(APIBase):
    def __init__(self, idn=None):
        super().__init__()
        if idn is not None:
            self.data = self.get(idn)
            self.getChildren()
        else:
            self.data = list(map(lambda o: Foo(o["Id"]).data, self.list()))

    def getChildren(self):
        if not self.data:
            return

        if type(self.data) == type(self):
            x = list(map(lambda o: o.getChildren(), self.data))
            self.data = x
        else:
            self.data["children"] = []
            for childId in self.data["childIds"]:
                a = AEntity(childId)
                b = BEntity(childId)
                self.data["children"].append(a.data and a.data or b.data)
        return self


class AEntity(APIBase):
    def __init__(self, idn=None):
        super().__init__()
        if idn is not None:
            self.data = self.get(idn)
            self.getChild()
        else:
            self.data = list(map(lambda o: AEntity(o["Id"]).data, self.list()))

    def getChild(self):
        if self.data:
            self.data["child"] = CEntity(self.data["cEntityId"]).data


class BEntity(APIBase):
    def __init__(self, idn=None):
        super().__init__()
        if idn is not None:
            self.data = self.get(idn)
            self.getChild()
        else:
            self.data = list(map(lambda o: BEntity(o["Id"]).data, self.list()))

    def getChild(self):
        if self.data:
            self.data["child"] = CEntity(self.data["cEntityId"]).data


class CEntity(APIBase):
    def __init__(self, idn):
        super().__init__()
        self.data = self.get(idn)



if __name__ == "__main__":
    logger = logging.getLogger(LOGBASE)
    logger.setLevel(LOGLEVEL)
    logger.handlers = []
    logger.addHandler(logging.StreamHandler())
    logger.handlers[-1].setFormatter(logging.Formatter("%(name)s: %(levelname)s: %(message)s"))

    import argparse
    from pprint import pprint

    parser = argparse.ArgumentParser()
    parser.add_argument("--loglevel", type=str, default="warn")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-f", help="get all Foos", action="store_const", const=Foo)
    group.add_argument("-a", help="get all AEntities", action="store_const", const=AEntity)
    group.add_argument("-b", help="get all BEntities", action="store_const", const=BEntity)
    args = parser.parse_args()

    logger.setLevel(getattr(logging, args.loglevel.upper()))

    if args.a: obj = args.a()
    elif args.b: obj = args.b()
    elif args.f: obj = args.f()

    pprint(obj.data, indent=2)

