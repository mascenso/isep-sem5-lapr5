import { Response, Request, NextFunction } from 'express';

exports.getMe = function(req: Request, res: Response) {
    console.log("Me?");
    // return back
    return res.json( req ).status(200);
}
