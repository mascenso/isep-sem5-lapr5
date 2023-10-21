import { Router } from 'express';
import auth from './routes/userRoute';
import user from './routes/userRoute';
import role from './routes/roleRoute';
import robot from './routes/robotRoute';
import building from './routes/buildingRoute';

export default () => {
	const app = Router();

	auth(app);
	user(app);
	role(app);
	robot(app);
  building(app);

	return app
}
