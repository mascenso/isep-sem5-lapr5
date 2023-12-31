import { Router } from 'express';
import task from './routes/taskRoute';
import routePlan from './routes/routePlanRoute';

export default () => {
	const app = Router();

	task(app);
	routePlan(app);

	return app
}
