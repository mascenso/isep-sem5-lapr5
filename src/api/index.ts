import { Router } from 'express';
import auth from './routes/userRoute';
import user from './routes/userRoute';
import role from './routes/roleRoute';
import robot from './routes/robotRoute';
import building from './routes/buildingRoute';
import floor from './routes/floorRoute';
import bridge from './routes/bridgeRoute';
import elevator from './routes/elevatorRoute';
import robotType from './routes/robotTypeRoute';

export default () => {
	const app = Router();

	auth(app);
	user(app);
	role(app);
	robot(app);
	robotType(app);
 	building(app);
	floor(app);
	bridge(app);
	elevator(app);

	return app
}
