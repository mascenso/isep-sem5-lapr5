import { Router } from 'express';
import robot from './routes/robotRoute';
import building from './routes/buildingRoute';
import floor from './routes/floorRoute';
import bridge from './routes/bridgeRoute';
import elevator from './routes/elevatorRoute';
import robotType from './routes/robotTypeRoute';
import room from './routes/roomRoute';


export default () => {
	const app = Router();

	robot(app);
	robotType(app);
	building(app);
	floor(app);
	bridge(app);
	elevator(app);
	room(app);

	return app
}
