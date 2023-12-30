import {jwtDecode} from "jwt-decode";
import {RobDroneGoToken} from "./robDroneGoToken";

const authRequest = (validRoles) => {
  return (req, res, next) => {
    const token = getTokenFromHeader(req);
    if (!token){
      return res.status(401).send("Unauthorized request");
    }
    try {
      const decodedToken = jwtDecode<RobDroneGoToken>(token);
      if (!validRoles.includes(decodedToken.role)
        || decodedToken.exp < (Date.now()/1000)) {
        return res.status(401).send("Unauthorized request");
      }
      next();
    } catch (error) {
      res.status(400).send("Invalid token!");
    }

  }
}

const getTokenFromHeader = req => {
  if (
    (req.headers.authorization && req.headers.authorization.split(' ')[0] === 'Token') ||
    (req.headers.authorization && req.headers.authorization.split(' ')[0] === 'Bearer')
  ) {
    return req.headers.authorization.split(' ')[1];
  }
  return null;
};

export default authRequest;

